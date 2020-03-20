## This module provides what might be called the most commonly recommended hash
## table in articles for novices.  It uses tombstone markers for deletion which
## allows a general ``probeSeq``.
##
## This solution is surely suboptimal for delete heavy workloads where tombs
## cost both space and time.  It is also likely suboptimal for any workload that
## actually triggers the perturbed probe sequence (due to its non-local memory
## access pattern).  Auto-fallback to scrambling aka rehashing hcodes is cheaper
## both in space and cache miss metrics against the *very same threat model* -
## namely "strong whole value user hash()es that are weak in some bits". Indeed,
## the perturbed sequence can be viewed as just a particularly expensive rehash
## that happens to not penalize the very first hop(s).
##
## I include this and the ordered variant ``otset`` here since people seem to
## want it due to in my view misunderstanding basic facts.  Invalid appeals to
## the authority of slow Python implementations do not help.  Also, the main Nim
## stdlib uses this presently.
##
## There is an improvement here to use a hybrid probe seq - less of a disaster,
## but still not as good as Robin Hood with rehash fallback on delete heavy
## workloads or just LP with rehash fallback.  Automatic rehash activation is
## disabled by default to assess efficacy of perturbed weak hash mitigation.
## If reactivated for production, it would be a "first line of defense" against
## weak hashes with perturbed probe sequences the second line, etc.

import althash, bitop, setops
type                  # `A` is Element type; Nim leans toward `item` language.
  HCell[A] = tuple[hcode: Hash, item: A]
  TSSet*[A] = object
    data: seq[HCell[A]]
    count: int            # count of entries
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

const free = Hash(0)
const tomb = Hash(-1)
proc isFree(hcode: Hash): bool {.inline.} = hcode == free
proc isTomb(hcode: Hash): bool {.inline.} = hcode == tomb
proc isUsed(hcode: Hash): bool {.inline.} = hcode != free and hcode != tomb

when defined(hashStats):    # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x   # all kind of like "times" - you v0=val;...; val-v0
  var tsDepth* = 0      ## Counts total search depth
  var tsTooFull* = 0    ## Counts resizes from minFree boundary
  var tsTooDeep* = 0    ## Counts resizes from deep probe sequences
  var tsTooSparse* = 0  ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(tsWarn) or not defined(danger):
  var tsWarn* = stderr  ## Set to wherever you want warnings to go
  var tsMaxWarn* = 10   ## Most warnings per program invocation
  var tsWarnCnt = 0     # Running counter of warnings issued

# s.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[A](s: TSSet[A], hc: Hash): Hash {.inline.} =
  if s.rehash: hash(hc, s.salt) else: hc

proc hash0[A](item: A): Hash {.inline.} =
  result = hash(item)   # Account for hash() returning zero, our FREE sentinel.
  if result == 0:       # Rarely taken branch very predictable
    result = 314159265  # Value matters little; More bits spread while enlarging
  elif result == tomb:
    result = 562951413  # Value matters little; More bits spread while enlarging

proc equal[A](s: TSSet[A], i: int, item: A, hc: Hash): bool {.inline.} =
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
    when defined(tsHybridProbe):
      if d * sz < 192:        # Linear probe for 192 bytes which is v.local
        i = (i + 1) and mask
      else:                   # Then v.non-local rotated hcode-perturbed probing.
        i = Hash((i.uint * 5 + 1 + pert) and mask.uint)
    else:
      i = Hash((i.uint * 5 + 1 + pert) and mask.uint)
    pert = pert shr 5                         # Decay to 0 => check whole table
    d.inc
    ifStats tsDepth.inc

proc rawGet[A](s: TSSet[A], item: A, hc0: Hash, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized TSSet")     # Adjust *caller* not here
  var t = -1                                        # Where to insert if missing
  var j: int
  for i in probeSeq(s.hashHc(hc0), s.data.high, d, s.data[0].sizeof):
    j = i
    if isTomb(s.data[i].hcode):                     # Tombstone
      if t == -1:                                   # First one
        t = i                                       # Remember insertions spot
    elif isFree(s.data[i].hcode):                   # Never used or empty seq
      break                                         # Stop looking
    elif s.equal(i, item, hc0):                     # Compare item to probe
      return i
    if d > s.data.len: # Depth-based resizing means that the table could be all
      break            # tombstones.  That might inf. loop without this check.
  if t == -1:
    t = j
  result = -1 - t

proc rawGetDeep[A](s: TSSet[A]; item: A; hc: Hash; d: var Hash): int {.inline.}=
  for i in probeSeq(s.hashHc(hc), s.data.high, d, s.data[0].sizeof):
    result = i
    if not s.data[i].hcode.isUsed:
      break
    d.inc
    ifStats tsDepth.inc

proc rawGet[A](s: TSSet[A], item: A; hc, d: var Hash): int {.inline.} =
  hc = hash0(item)
  let hc0 = hc
  rawGet(s, item, hc0, d)

proc rawGet[A](s: TSSet[A], item: A): int {.inline.} =
  var hc, d: Hash
  rawGet(s, item, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc depth[A](s: TSSet[A]; item: A): int {.inline.} =
  var hc, d: Hash
  discard rawGet(s, item, hc, d)
  d

proc tooFull[A](s: TSSet[A], d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if s.data.len - s.count < s.minFree.int + 1:
    dbg echo("Too little space (", s.data.len-s.count,") of ",s.data.len)
    ifStats tsTooFull.inc
    newSize = s.data.len shl s.growPow2
    return
  if s.denom.int * (d - 1) < s.numer.int * s.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",s.pow2," depths: ",s.depths)
  ifStats tsTooDeep.inc
  if s.count > s.data.len shr s.growPow2:
    newSize = s.data.len shl s.growPow2
  else:
    dbg echo("Too sparse to grow, ",s.count,"/",s.data.len," depth: ",d)
    ifStats tsTooSparse.inc     # Normal resizing cannot restore performance
    var ext: string             # extra text after primary message
    if s.rehash:
      ext = "; Switch to tree|seq for dups"
      result = false          # Could potentially auto-convert to B-tree here
      newSize = s.data.len    #XXX Try 2nd kind of rehash|maybe a `hash2()`?
    else:
      when defined(tsAutoRehash):
        ext = "; Adapting by re-hashing hash()"
        s.rehash = true
        newSize = s.data.len
      else:
        discard
    when defined(tsWarn) or not defined(danger):
      tsWarnCnt.inc
      if tsWarnCnt <= tsMaxWarn:
        tsWarn.write "TSSet: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut[A](s: var TSSet[A], i: Hash): int {.inline.} =
  result = i

proc rawDel[A](s: var TSSet[A], i: Hash) {.inline.} =
  s.data[i].hcode = tomb                # Update hcode to be tomb marker
  s.data[i].item = default(A)           # Update item to be non-ref holding val

# From here down is very similar for any open addr table (maybe w/getData(i)..)
# except for unkeyed-`pop` and `allItems` which do care about internal layout.
template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0: s.init
  var hc, d, newSize: Hash
  var i = s.rawGet(item, hc, d)
  if i < 0:
    if s.tooFull(d, newSize):
      s.setCap newSize
      d = 0
      i = s.rawGet(item, hc, d)
    let k = s.rawPut(-1 - i)            # Allocate a slot
    s.count.inc
    s.data[k].hcode = hc
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0: missing
  var hc, d: Hash
  let i = s.rawGet(item, hc, d)
  if i >= 0:
    s.data[i].hcode = tomb
    s.count.dec
    present
  else:
    missing

var tsInitialSize* = 4 ## default initial size aka capacity aka cap
var tsNumer*       = 3 ## default numerator for lg(n) probe depth limit
var tsDenom*       = 2 ## default denominator for lg(n) probe depth limit
var tsMinFree*     = 1 ## default min free slots; (>= 1)
var tsGrowPow2*    = 1 ## default growth power of 2; 1 means double
var tsRehash*      = true ## default hcode rehashing behavior
var tsRobinHood*   = false ## default to Robin Hood re-org on insert/delete

proc init*[A](s: var TSSet[A], initialSize=tsInitialSize, numer=tsNumer,
              denom=tsDenom, minFree=tsMinFree, growPow2=tsGrowPow2,
              rehash=tsRehash, robinhood=tsRobinHood) {.inline.} =
  s.data     = newSeq[HCell[A]](initialSize)
  s.count    = 0
  s.salt     = getSalt(s.data[0].addr)
  s.numer    = numer.uint8
  s.denom    = denom.uint8
  s.minFree  = max(1, minFree).uint8 # Block user allowing inf loop possibility
  s.growPow2 = uint8(growPow2)
  s.pow2     = uint8(initialSize.lg)
  s.rehash   = rehash
  s.robin    = robinhood

proc initTSSet*[A](initialSize=tsInitialSize, numer=tsNumer, denom=tsDenom,
                   minFree=tsMinFree, growPow2=tsGrowPow2, rehash=tsRehash,
                   robinhood=tsRobinHood): TSSet[A] {.inline.}=
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[A](s: var TSSet[A], numer=tsNumer, denom=tsDenom,
                   minFree=tsMinFree, growPow2=tsGrowPow2, rehash=tsRehash,
                   robinhood=tsRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  s.numer    = numer
  s.denom    = denom
  s.minFree  = minFree
  s.growPow2 = growPow2
  s.rehash   = rehash    
  s.robin    = robinhood 

proc rightSize*(count: int, numer=tsNumer, denom=tsDenom, minFree=tsMinFree):
    int {.inline.} = ceilPow2(count * 4 div 3 + minFree) # Might have good hash

proc len*[A](s: TSSet[A]): int {.inline.} = s.count

proc getCap*[A](s: TSSet[A]): int {.inline.} = s.data.len

proc setCap*[A](s: var TSSet[A], newSize = -1) =
  if s.data.len == 0: s.init
  var newSz: int
  if newSize < 0:
    newSz = s.data.len shl s.growPow2
    s.pow2 += s.growPow2
  else:
    newSz = max(newSize, rightSize(s.data.len, minFree=s.minFree.int))
    s.pow2 = uint8(newSz.lg)
  dbg echo("RESIZE@ ",s.count,"/",s.data.len," ",s.data.len.float/s.data.len.float)
  var old: seq[HCell[A]]
  var d: Hash
  newSeq(old, newSz)
  if s.rehash: s.salt = getSalt(old[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", s.salt)
  swap(s.data, old)
  for i, cell in old:
    d = 0
    let j = s.rawGetDeep(cell.item, cell.hcode, d)
    s.data[s.rawPut(j)] = move(old[i])

proc contains*[A](s: TSSet[A], item: A): bool {.inline.} =
  if s.data.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var TSSet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k].item = item

proc setOrIncl*[A](s: var TSSet[A], item: A): bool {.inline.} =
  getPut do: s.data[i].item = item; result = true
  do       : s.data[k].item = item; result = false

proc mgetOrIncl*[A](s: var TSSet[A], item: A, had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[i].item
  do       : had = false; s.data[k].item = item; return s.data[k].item

proc add*[A](s: var TSSet[A], item: A) {.inline.} =
  if s.data.len == 0: s.init
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
  s.count.inc

template withItem*[A](s: var TSSet[A], itm: A, it, body: untyped) =
  var i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].item.addr
    body

template withItem*[A](s: var TSSet[A], itm: A, it, body1, body2: untyped) =
  var i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].item.addr
    body1
  else:
    body2

template withItem*[A](s: TSSet[A], itm: A, it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i].item
    body1
  else:
    body2

proc missingOrExcl*[A](s: var TSSet[A], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A](s: var TSSet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[i].item); s.rawDel i; return true
  do: return false

proc pop*[A](s: var TSSet[A]): var A {.inline.} =
  if s.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  for e in s:         # Cheaper than it may look due to early exit, BUT can get
    result = move(e)  #..slow as a set empties out.  Could keep running "min ix"
    s.excl result     #..based on s.flag (cheaply updated by either ins/del.
    return            #..Also broken if dups are present. XXX

proc clear*[A](s: var TSSet[A]) =
  if s.data.len == 0: return
  s.count = 0
  zeroMem s.data[0].addr, s.data.len * s.data[0].sizeof

iterator items*[A](s: TSSet[A]): A =
  let L = s.len
  for cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield cell.item

iterator mitems*[A](s: var TSSet[A]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i].item

iterator pairs*[A](s: TSSet[A]): tuple[a: int, b: A] =
  let L = s.len
  for i, cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield (i, cell.item)

iterator hcodes*[A](s: TSSet[A]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.data[i].hcode.isUsed: yield (i, s.data[i].hcode)

iterator allItems*[A](s: TSSet[A]; item: A): A =
  let L = s.len
  if L > 0:
    let hc0 = hash0(item)
    var d: Hash
    var did: seq[Hash]              #probeSeq can yield same i twice or more!
    for i in probeSeq(s.hashHc(hc0), s.data.high, d, s.data[0].sizeof):
      if s.equal(i, item, hc0) and i notin did:
        assert(s.len == L, "cannot change a set while iterating over it")
        yield s.data[i].item
        did.add i

proc debugDump*[A](s: TSSet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  echo "TABLE: "
  for i, cell in s.data:
    let hc = cell.hcode
    echo "i: ", i, " depth: ", if hc.isUsed: s.depth(s.data[i].item) else: -1,
         " ", if hc.isTomb: "T" else: $cell

defSetOps(TSSet)
