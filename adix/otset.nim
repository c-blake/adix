## This module provides a >=2-fetch indirect, insertion-ordered compact table
## like ``olset`` (see comments there), but using the hash table approach of
## ``tsset`` (see comments there).

import althash, bitop, setops
const tomb = 0xFFFFFFFF'u32     # Could ==1, w/0=free & - 2 vs - 1 idx shift

type                  # `A` is Element type; Nim leans toward `item` language.
  HCell[A] = tuple[hcode: Hash, item: A]
  OTSet*[A] = object
    data: seq[HCell[A]]
    idx: seq[uint32]      #TODO save space via n-Byte ptrs for idx, n*i..n*i+n-1
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

when defined(hashStats):    # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x   # all kind of like "times" - you v0=val;...; val-v0
  var otDepth* = 0      ## Counts total search depth
  var otTooFull* = 0    ## Counts resizes from minFree boundary
  var otTooDeep* = 0    ## Counts resizes from deep probe sequences
  var otTooSparse* = 0  ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(otWarn) or not defined(danger):
  var otWarn* = stderr  ## Set to wherever you want warnings to go
  var otMaxWarn* = 10   ## Most warnings per program invocation
  var otWarnCnt = 0     # Running counter of warnings issued

proc len*[A](s: OTSet[A]): int {.inline.} = s.data.len

# s.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[A](s: OTSet[A], hc: Hash): Hash {.inline.} =
  if s.rehash: hash(hc, s.salt) else: hc

proc hash0[A](item: A): Hash {.inline.} =
  result = hash(item)   # Account for hash() returning zero, our FREE sentinel.
  if result == 0:       # Rarely taken branch very predictable
    result = 314159265  # Value matters little; More bits spread while enlarging

proc equal[A](s: OTSet[A], i: uint32, item: A, hc: Hash): bool {.inline.} =
  when type(item) is SomeInteger:             # Do not waste time on `hc` cmp
    s.data[i].item == item
  elif compiles(item.key):
    when type(item.key) is SomeInteger: #XXX Should be tabkey.getKey or a user
      s.data[i].item.key == item.key    #XXX ..overload if "key" not actual key
    else:
      s.data[i].hcode == hc and s.data[i].item == item
  else:   # Prefix compare hc so missing => ~0 item cmp; Present => ~1 item cmp
    s.data[i].hcode == hc and s.data[i].item == item

iterator probeSeq(hc, mask: Hash, d: var Hash, sz: int): int =
  var i: Hash = hc and mask                   # Start w/home address
  var pert = rotateRightBits(hc.uint, 4*Hash.sizeof)  # Rotate 1/2 machine word
  d = 0
  while true:
    yield i
    # tsHybridProbe is @timotheecour's fine idea improved to be in units of
    # memory rather than number of data slots.  Limit should be several cache
    # lines and maybe a define/runtime parameter conditioned upon which CPU.
    when defined(otHybridProbe):
      if d * sz < 192:        # Linear probe for 192 bytes which is v.local
        i = (i + 1) and mask
      else:                   # Then non-local rotated hcode-perturbed probing.
        i = Hash((i.uint * 5 + 1 + pert) and mask.uint)
        pert = pert shr 5     # Decay to 0 => check whole table
    else:                     # The Python probe sequence (up to `pert`)
      i = Hash((i.uint * 5 + 1 + pert) and mask.uint)
      pert = pert shr 5       # Decay to 0 => check whole table
    d.inc
    ifStats tsDepth.inc

proc rawGet[A](s: OTSet[A], item: A, hc0: Hash, d: var Hash): int {.inline.} =
  assert(s.idx.len > 0, "Uninitialized OTSet")      # Adjust *caller* not here
  var t = -1                                        # Where to insert if missing
  var j: int
  for i in probeSeq(s.hashHc(hc0), s.idx.high, d, s.idx[0].sizeof):
    j = i
    if s.idx[i] == tomb:                            # Tombstone
      if t == -1:                                   # First one
        t = i                                       # Remember insertions spot
    elif s.idx[i] == 0:                             # Never used
      break                                         # Stop looking
    elif s.equal(s.idx[i] - 1, item, hc0):          # Compare item to probe
      return i
    if d > s.idx.len: # Depth-based resizing means that the table could be all
      break           # tombstones.  That might inf. loop without this check.
  if t == -1:
    t = j
  result = -1 - t

proc rawGet[A](s: OTSet[A], item: A; hc, d: var Hash): int {.inline.} =
  hc = hash0(item)
  let hc0 = hc
  rawGet(s, item, hc0, d)

proc rawGetDeep[A](s: OTSet[A]; item: A; hc: Hash; d: var Hash): int {.inline.}=
  for i in probeSeq(s.hashHc(hc), s.idx.high, d, s.idx[0].sizeof):
    result = i
    if s.idx[i] == 0:
      break

proc rawGetLast[A](s: OTSet[A]): int {.inline.} =
  let hc = s.data[^1].hcode
  var d: Hash
  for i in probeSeq(s.hashHc(hc), s.idx.high, d, s.idx[0].sizeof):
    if s.idx[i].int == s.data.len: return i

proc rawGet[A](s: OTSet[A], item: A): int {.inline.} =
  var hc, d: Hash
  rawGet(s, item, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc depth[A](s: OTSet[A]; item: A): int {.inline.} =
  var hc, d: Hash
  discard rawGet(s, item, hc, d)
  d

proc tooFull[A](s: OTSet[A], d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if s.idx.len - s.data.len < s.minFree.int + 1:
    dbg echo("Too little space (",s.idx.len-s.data.len,") of ",s.idx.len)
    ifStats otTooFull.inc
    newSize = s.idx.len shl s.growPow2
    return
  if s.denom.int * (d - 1) < s.numer.int * s.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",s.pow2," depths: ",s.depths)
  ifStats otTooDeep.inc
  if s.data.len > s.idx.len shr s.growPow2:
    newSize = s.idx.len shl s.growPow2
  else:
    dbg echo("Too sparse to grow, ",s.data.len,"/",s.idx.len," depth: ",d)
    ifStats otTooSparse.inc     # Normal resizing cannot restore performance
    var ext: string             # extra text after primary message
    if s.rehash:
      ext = "; Switch to tree|seq for dups"
      result = false          # Could potentially auto-convert to B-tree here
      newSize = s.idx.len    #XXX Try 2nd kind of rehash|maybe a `hash2()`?
    else:
      when defined(otAutoRehash):
        ext = "; Adapting by re-hashing hash()"
        s.rehash = true
        newSize = s.idx.len
      else:
        discard
    when defined(otWarn) or not defined(danger):
      otWarnCnt.inc
      if otWarnCnt <= otMaxWarn:
        otWarn.write "OTSet: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut[A](s: var OTSet[A], i: Hash): int {.inline.} =
  s.idx[i] = s.len.uint32 + 1
  s.data.setLen s.len + 1
  return s.len - 1

proc rawDel[A](s: var OTSet[A], i: Hash) {.inline.} =
  let j = s.idx[i] - 1                  # To preserve `data` density find idx
  if j.int + 1 < s.len:                 #..pointing to last element.
    let k = s.rawGetLast
    s.idx[k]  = j + 1                   # Point instead to `j` element
    s.data[j] = s.data[^1]              # And replace `[j]` with last elt; move?
  s.idx[i] = tomb                       # Update idx(end item) to be tombstone
  discard s.data.pop

# From here down is very similar for any open addr table (maybe w/getData(i)..)
# except for unkeyed-`pop` and `allItems` which do care about internal layout.
template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.idx.len == 0: s.init
  var hc, d, newSize: Hash
  var i = s.rawGet(item, hc, d)
  if i < 0:
    if s.tooFull(d, newSize):
      s.setCap newSize
      d = 0
      i = s.rawGet(item, hc, d)
    let k = s.rawPut(-1 - i)            # Allocate a slot
    s.data[k].hcode = hc
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.idx.len == 0: missing
  var hc, d: Hash
  let i = s.rawGet(item, hc, d)
  if i >= 0: present
  else: missing

var otInitialSize* = 4 ## default initial size aka capacity aka cap
var otNumer*       = 2 ## default numerator for lg(n) probe depth limit
var otDenom*       = 1 ## default denominator for lg(n) probe depth limit
var otMinFree*     = 1 ## default min free slots; (>= 1)
var otGrowPow2*    = 1 ## default growth power of 2; 1 means double
var otRehash*      = true ## default hcode rehashing behavior
var otRobinHood*   = false ## default to Robin Hood re-org on insert/delete

proc slotsGuess(count: int, numer=otNumer, denom=otDenom, minFree=otMinFree):
    int {.inline.} = ceilPow2(count * 4 div 3 + minFree)

proc init*[A](s: var OTSet[A], initialSize=otInitialSize, numer=otNumer,
              denom=otDenom, minFree=otMinFree, growPow2=otGrowPow2,
              rehash=otRehash, robinhood=otRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize)
  s.data     = newSeqOfCap[HCell[A]](initialSize * 3 div 4)
  s.idx      = newSeq[uint32](initialSize) #WTF --gc:arc bug w/incompat structs
  s.salt     = getSalt(s.idx[0].addr)
  s.numer    = numer.uint8
  s.denom    = denom.uint8
  s.minFree  = max(1, minFree).uint8 # Block user allowing inf loop possibility
  s.growPow2 = uint8(growPow2)
  s.pow2     = uint8(initialSize.lg)
  s.rehash   = rehash
  s.robin    = robinhood

proc initOTSet*[A](initialSize=otInitialSize, numer=otNumer, denom=otDenom,
                   minFree=otMinFree, growPow2=otGrowPow2, rehash=otRehash,
                   robinhood=otRobinHood): OTSet[A] {.inline.}=
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[A](s: var OTSet[A], numer=otNumer, denom=otDenom,
                   minFree=otMinFree, growPow2=otGrowPow2, rehash=otRehash,
                   robinhood=otRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  s.numer    = numer
  s.denom    = denom
  s.minFree  = minFree
  s.growPow2 = growPow2
  s.rehash   = rehash    
  s.robin    = robinhood 

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc getCap*[A](s: OTSet[A]): int {.inline.} = s.idx.len

proc setCap*[A](s: var OTSet[A], newSize = -1) =
  if s.idx.len == 0: s.init
  var newSz: int
  if newSize < 0:
    newSz = s.idx.len shl s.growPow2
    s.pow2 += s.growPow2
  else:
    newSz = max(newSize, slotsGuess(s.data.len, minFree=s.minFree.int))
#   if newSz == s.idx.len: return # may still be too full of tombstones
    s.pow2 = uint8(newSz.lg)
  dbg echo("RESIZE@ ",s.data.len,"/",s.idx.len," ",s.data.len.float/s.idx.len.float)
  s.idx = newSeq[uint32](newSz)
  if s.rehash: s.salt = getSalt(s.idx[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", s.salt)
  var d: Hash
  for i, cell in s.data:
    d = 0
    let j = s.rawGetDeep(cell.item, cell.hcode, d)
    s.idx[j] = (i + 1).uint32

proc contains*[A](s: OTSet[A], item: A): bool {.inline.} =
  if s.idx.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var OTSet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k].item = item

proc setOrIncl*[A](s: var OTSet[A], item: A): bool {.inline.} =
  getPut do: s.data[s.idx[i] - 1].item = item; result = true
  do       : s.data[k].item = item           ; result = false

proc mgetOrIncl*[A](s: var OTSet[A], item: A, had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[s.idx[i] - 1].item
  do       : had = false; s.data[k].item = item; return s.data[k].item

proc add*[A](s: var OTSet[A], item: A) {.inline.} =
  if s.idx.len == 0: s.init
  let hc = hash0(item)
  var d, newSize: Hash
  var i = s.rawGetDeep(item, hc, d)
  if s.tooFull(d, newSize):
    s.setCap newSize
    d = 0
    i = s.rawGetDeep(item, hc, d)
  let k = s.rawPut(i)                     # Allocate a slot
  s.data[k].hcode = hc
  s.data[k].item = item

template withItem*[A](s: var OTSet[A], itm: A; it,body1: untyped; body2: untyped=nil) =
  var i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[s.idx[i] - 1].item.addr
    body1
  else:
    body2

template withItem*[A](s: OTSet[A], itm: A; it,body1: untyped; body2: untyped=nil) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i].item.unsafeAddr
    body1
  else:
    body2

proc missingOrExcl*[A](s: var OTSet[A], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A](s: var OTSet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[s.idx[i] - 1].item); s.rawDel i; return true
  do: return false

proc pop*[A](s: var OTSet[A]): A =
  if s.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  let i = s.rawGetLast                  # Find idx[] pointing to last element
  result = move(s.data[^1])             # seq pop avoids O(len) shift on data
  s.rawDel i                            # does the s.data.pop

proc clear*[A](s: var OTSet[A]) =
  if s.idx.len == 0: return
  s.data.setLen 0
  zeroMem s.idx[0].addr, s.idx.len * s.idx[0].sizeof

iterator items*[A](s: OTSet[A]): A =
  let L = s.len
  for cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield cell.item

iterator mitems*[A](s: var OTSet[A]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i].item

iterator pairs*[A](s: OTSet[A]): tuple[a: int, b: A] =
  let L = s.len
  for i, cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield (i, cell.item)

iterator hcodes*[A](s: OTSet[A]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.idx.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.idx[i] != 0 and s.idx[i] != tomb: yield (i, s.data[i].hcode)

iterator allItems*[A](s: OTSet[A]; item: A): A =
  let L = s.len
  if L > 0:
    let hc0 = hash0(item)
    var d: Hash
    var did: seq[Hash]              #probeSeq can yield same i twice or more!
    for i in probeSeq(s.hashHc(hc0), s.idx.high, d, s.idx[0].sizeof):
      if s.equal(s.idx[i] - 1, item, hc0) and i notin did:
        assert(s.len == L, "cannot change a set while iterating over it")
        did.add i
        yield s.data[i].item

proc debugDump*[A](s: OTSet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  echo "data: ", s.data
  echo "idx:"
  for i, j in s.idx:
    echo "i: ", i, " depth: ",
         if j == 0: 0   elif j == tomb: -1  else: s.depth(s.data[j-1].item)," ",
         if j == 0: "0" elif j == tomb: "T" else: $s.data[j-1]

defSetOps(OTSet)
