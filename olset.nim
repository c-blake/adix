## This module provides indirect, insertion-ordered compact sets.  The idea is a
## hybrid of direct indexing & ordinary hashing, i.e. keep a dense ``seq`` of
## insertion-ordered records and an ``lptab[ptr A, int]`` that indexes it.  This
## is obviously optimally fast at iterating over elements in insertion order,
## but also obviously slower on random access than unordered hash sets due to
## extra indirection (which may not matter for L1/L2 resident sets which may be
## the dominant application of "insertion ordered" sets in, e.g. compilers).

# It seems difficult to have the "notional" ``ptr A`` resolved via indirecting
# into an ``s.data`` only for the purposes of the table without a very different
# ``Table`` interface.  So, ``lpset.nim`` code is duplicated here with slight
# amendments needed to become indirect to avoid include/templates.
#
# While not (yet) done here, it is possible to optimize this to include partial
# hcodes in ``idx`` entries (tuple of those & data index, basically) and do the
# prefix compare on the partial hcode.  This doubles the size of the index but
# allows a single cache line fetch (or a few consecutive) to decide the second
# fetch into the data array for a final key comparison.  The space of this (at
# some time cost) can be further optimized by using ``sequint`` and only storing
# a pair of B-bit numbers (B=s.idx.len.lgCeil).  For now we just use a 4 byte
# ``uint32`` for the index supporting 4 GiItems.  Because the storage optimized
# B-bit table is a little slower than a ``seq``, a compile-time switch or wholly
# distinct type might be best.

import althash, bitop, memutil, setops
type                  # `A` is Element type; Nim leans toward `item` language.
  HCell[A] = tuple[hcode: Hash, item: A]
  OLSet*[A] = object                          ## Robin Hood Hash Set
    data: seq[HCell[A]]                       # data array
    idx: seq[uint32]                  #TODO optimize to 2B bit (hcode,ix) pairs
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

proc isUsed(idx: seq[uint32], i: int): bool {.inline.} = idx[i] != 0

when defined(hashStats):    # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x   # all kind of like "times" - you v0=val;...; val-v0
  var olDepth* = 0      ## Counts total search depth
  var olTooFull* = 0    ## Counts resizes from minFree boundary
  var olTooDeep* = 0    ## Counts resizes from deep probe sequences
  var olTooSparse* = 0  ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(olWarn) or not defined(danger):
  var olWarn* = stderr  ## Set to wherever you want warnings to go
  var olMaxWarn* = 10   ## Most warnings per program invocation
  var olWarnCnt = 0     # Running counter of warnings issued

# s.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[A](s: OLSet[A], hc: Hash): Hash {.inline.} =
  if s.rehash: hash(hc, s.salt) else: hc

proc hash[A](s: OLSet[A], i: int): Hash {.inline.} =
  s.hashHc s.data[s.idx[i] - 1].hcode

proc hash0[A](item: A): Hash {.inline.} =
  result = hash(item)   # Account for hash() returning zero, our FREE sentinel.
  if result == 0:       # Rarely taken branch very predictable
    result = 314159265  # Value matters little; More bits spread while enlarging

proc equal[A](s: OLSet[A], i: uint32, item: A, hc: Hash): bool {.inline.} =
  when type(item) is SomeInteger:             # Do not waste time on `hc` cmp
    s.data[i].item == item
  elif compiles(item.key):
    when type(item.key) is SomeInteger: #XXX Should be tabkey.getKey or a user
      s.data[i].item.key == item.key    #XXX..overload if "key" not actual key.
    else:
      s.data[i].hcode == hc and s.data[i].item == item
  else:   # Prefix compare hc so missing => ~0 item cmp; Present => ~1 item cmp
    s.data[i].hcode == hc and s.data[i].item == item

proc depth(i, hc, mask: Hash): Hash {.inline.} =
  let i = uint(i)
  let hc = uint(hc)
  let mask = uint(mask)
  Hash((i - hc) and mask)                 # Search depth of entry w/hcode @ix`i`

iterator probeSeq(hc, mask: Hash): int =  # WTF generic over A caused codegenbug
  var i: Hash = hc and mask               # Start w/home address
  while true:
    yield i
    i = (i + 1) and mask                  # Linear Probing

proc rawGet[A](s: OLSet[A]; item: A; hc, d: var Hash): int {.inline.} =
  assert(s.idx.len > 0, "Uninitialized OLSet")  # Ensure in *caller* not here
  hc = hash0(item)
  var t {.noInit.}: int                         # Where to insert if missing
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    t = i
    if not isUsed(s.idx, i):                 # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.idx.high):
      break
    if s.equal(s.idx[i] - 1, item, hc):
      return i
    d.inc
    ifStats olDepth.inc
  result = -1 - t               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep[A](s: OLSet[A]; item: A; hc: Hash; d: var Hash): int {.inline.}=
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    result = i
    if not isUsed(s.idx, i):                 # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.idx.high):
      break
    d.inc
    ifStats olDepth.inc

proc rawGetLast[A](s: OLSet[A]): int {.inline.} =
  let hc = s.data[^1].hcode
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    if s.idx[i].int == s.data.len: return i

proc rawGet[A](s: OLSet[A], item: A): int {.inline.} =
  var hc, d: Hash
  rawGet(s, item, hc, d)

proc depth[A](s: OLSet[A]; item: A): int {.inline.} =
  var hc, d: Hash
  discard rawGet(s, item, hc, d)
  d

proc tooFull[A](s: var OLSet[A], d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if s.idx.len - s.data.len < s.minFree.int + 1:
    dbg echo("Too little space (",s.idx.len-s.data.len,") of ",s.idx.len)
    ifStats lpTooFull.inc
    newSize = s.idx.len shl s.growPow2
    return
  if s.denom.int * (d - 1) < s.numer.int * s.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",s.pow2," depths: ",s.depths)
  ifStats lpTooDeep.inc
  if s.data.len > s.idx.len shr s.growPow2:
    newSize = s.idx.len shl s.growPow2
  else:
    dbg echo("Too sparse to grow, ",s.data.len,"/",s.idx.len," depth: ",d)
    ifStats lpTooSparse.inc     # Normal resizing cannot restore performance
    newSize = s.idx.len
    var ext: string             # extra text after primary message
    if s.robin:                 # Robin Hood already active
      if s.rehash:              # rehashing hash() already active
        ext = "; Switch to tree|seq for dups"
        result = false          # Could potentially auto-convert to B-tree here
      else:
        ext = "; Adapting by re-hashing hash()"
        s.rehash = true
    else:                       # Turn on Robin Hood
      s.robin = true
      ext = "; Adapting by Robin Hood re-org"
    when defined(olWarn) or not defined(danger):
      olWarnCnt.inc
      if olWarnCnt <= olMaxWarn:
        olWarn.write "OLSet: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1[A](s: var OLSet[A], i: Hash; d: var int): int {.inline.} =
  if s.robin:
    var j = i                           # Linear probe to first empty slot
    while isUsed(s.idx, j):
      j = (j + 1) and s.idx.high
      d.inc

proc rawPut2[A](s: var OLSet[A], i, j: Hash): int {.inline.} =
  if s.robin:
    if j > i:                           # No table wrap around; just shift up
      pushUp s.idx, i, j - i
    elif j < i:                         # j wrapped to low indices
      pushUp s.idx, 0, j
      s.idx[0] = s.idx[s.idx.high]
      pushUp s.idx, i, s.idx.high - i
# else:                                 # j == i => already have space @i; done
  s.idx[i] = s.data.len.uint32 + 1
  s.data.setLen s.data.len + 1
  s.data.len - 1

proc rawDelIdx[A](s: var OLSet[A], i: Hash) {.inline.} =
  let mask = s.idx.high
  if s.robin:
    var k = i
    var j = (i + 1) and mask            # Find next empty|at home position entry
    while isUsed(s.idx, j) and j != (s.hash(j) and mask):
      j = (j + 1) and mask
    if j > i + 1:                       # No table wrap around; just shift down
      pullDown s.idx, i, j - 1 - i
      k = j - 1                         # Mark just-past-shift-block entry empty
    elif ((j + mask - i) and mask) > 0: # j wrapped to low indices; Did >0 j.inc
      pullDown s.idx, i, mask - i
      s.idx[mask] = s.idx[0]
      pullDown s.idx, 0, j - 1
      k = (j + mask) and mask           # [j-1 mod tabSz] is now empty
#   else:                               # k == i is already home position
    s.idx[k] = 0                        # mark FREE
  else:
    var i = i             # KnuthV3 Algo6.4R adapted for i=i+1 instead of i=i-1
    while true:           # The correctness of this depends on (i+1) in probeSeq
      var j = i           #..though may be adaptable to other simple sequences.
      s.idx[i] = 0                      # Mark current FREE
      while true:
        i = (i + 1) and mask            # Increment mod table size
        if not isUsed(s.idx, i):        # End of collision cluster; All done
          return
        let h = s.hash(i) and mask      # "home" slot of key@i
        if not ((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
          break
      s.idx[j] = s.idx[i]               # idx[i] will be marked FREE next loop

proc rawDel[A](s: var OLSet[A], i: Hash) {.inline.} =
  let j = s.idx[i] - 1                  # To preserve `data` density find idx
  if j.int + 1 < s.len:                 #..pointing to last element.
    let k = s.rawGetLast
    s.idx[k]  = j + 1                   # Point instead to `j` element
    s.data[j] = s.data[^1]              # And replace `[j]` with last elt; move?
  s.rawDelIdx i                         # Delete from index
  discard s.data.pop

# From here down is very similar for any open addr table (maybe w/getData(i)..)
# except for unkeyed-`pop` and `allItems` which do care about internal layout.
template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.idx.len == 0: s.init
  var hc, d, newSize: Hash
  var i = s.rawGet(item, hc, d)
  if i < 0:
    var j = s.rawPut1(-1 - i, d)
    if s.tooFull(d, newSize):
      s.setCap
      d = 0
      i = s.rawGet(item, hc, d)
      j = s.rawPut1(-1 - i, d)
    let k = s.rawPut2(-1 - i, j)        # Maybe allocate a slot
    s.data[k].hcode = hc
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0:
    missing
  let i = s.rawGet(item)
  if i >= 0:
    present
  else:
    missing

var olInitialSize* = 4 ## default initial size aka capacity aka cap
var olNumer*       = 3 ## default numerator for lg(n) probe depth limit
var olDenom*       = 2 ## default denominator for lg(n) probe depth limit
var olMinFree*     = 1 ## default min free slots; (>= 1)
var olGrowPow2*    = 1 ## default growth power of 2; 1 means double
var olRehash*      = false ## default hcode rehashing behavior; disable@own peril
var olRobinHood*   = false ## default to Robin Hood re-org on insert/delete

proc init*[A](s: var OLSet[A], initialSize=olInitialSize, numer=olNumer,
              denom=olDenom, minFree=olMinFree, growPow2=olGrowPow2,
              rehash=olRehash, robinhood=olRobinHood) {.inline.} =
  s.data     = newSeqOfCap[HCell[A]]((initialSize div 4 + 1) * 3)
  s.idx      = newSeq[uint32](initialSize) #WTF --gc:arc bug w/incompat structs
  s.salt     = getSalt(s.idx[0].addr)
  s.numer    = numer.uint8
  s.denom    = denom.uint8
  s.minFree  = max(1, minFree).uint8 # Block user allow inf loop possibility
  s.growPow2 = uint8(growPow2)
  s.pow2     = uint8(initialSize.lg)
  s.rehash   = rehash
  s.robin    = robinhood

proc initOLSet*[A](initialSize=olInitialSize, numer=olNumer, denom=olDenom,
                   minFree=olMinFree, growPow2=olGrowPow2, rehash=olRehash,
                   robinhood=olRobinHood): OLSet[A] {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[A](s: var OLSet[A], numer=olNumer, denom=olDenom,
                   minFree=olMinFree, growPow2=olGrowPow2, rehash=olRehash,
                   robinhood=olRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  s.numer    = numer
  s.denom    = denom
  s.minFree  = minFree
  s.growPow2 = growPow2
  s.rehash   = rehash
  s.robin    = robinhood

proc rightSize*(count: int, numer=olNumer, denom=olDenom, minFree=olMinFree):
    int {.inline.} = ceilPow2(count + minFree) # Might have a great hash

proc len*[A](s: OLSet[A]): int {.inline.} = s.data.len

proc getCap*[A](s: OLSet[A]): int {.inline.} = s.idx.len

proc setCap*[A](s: var OLSet[A], newSize = -1) =
  if s.idx.len == 0: s.init
  var newSz: int
  if newSize < 0:
    newSz = s.idx.len shl s.growPow2
    s.pow2 += s.growPow2
  else:
    newSz = max(newSize, rightSize(s.data.len, minFree=s.minFree.int))
    s.pow2 = uint8(newSz.lg)
  if newSz == s.idx.len and newSize == -1:
    return
  dbg echo("RESIZE@ ",s.data.len,"/",s.idx.len," ",s.data.len.float/s.idx.len.float)
  s.idx = newSeq[uint32](newSz)
  if s.rehash: s.salt = getSalt(s.idx[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", s.salt)
  for i, cell in s.data:
    var d: Hash = 0
    let j = s.rawGetDeep(cell.item, cell.hcode, d)
    s.idx[j] = (i + 1).uint32

proc contains*[A](s: OLSet[A], item: A): bool {.inline.} =
  if s.data.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var OLSet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k].item = item

proc setOrIncl*[A](s: var OLSet[A], item: A): bool {.inline.} =
  getPut do: s.data[s.idx[i] - 1].item = item; result = true
  do       : s.data[k].item = item           ; result = false

proc mgetOrIncl*[A](s: var OLSet[A], item: A, had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[s.idx[i] - 1].item
  do       : had = false; s.data[k].item = item; return s.data[k].item

proc add*[A](s: var OLSet[A], item: A) {.inline.} =
  if s.idx.len == 0: s.init
  let hc = hash0(item)
  var d, newSize: Hash
  var i = s.rawGetDeep(item, hc, d)
  var j = s.rawPut1(i, d)
  if s.tooFull(d, newSize):
    s.setCap newSize
    d = 0
    i = s.rawGetDeep(item, hc, d)
    j = s.rawPut1(i, d)
  let k = s.rawPut2(i, j)               # Maybe allocate a slot
  s.data[k].hcode = hc
  s.data[k].item  = item

template withItem*[A](s: var OLSet[A], itm: A, it, body: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[s.idx[i] - 1].item.addr
    body

template withItem*[A](s: var OLSet[A], itm: A, it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[s.idx[i] - 1].item.addr
    body1
  else:
    body2

template withItem*[A](s: OLSet[A], itm: A, it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i].item
    body1
  else:
    body2

proc missingOrExcl*[A](s: var OLSet[A], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A](s: var OLSet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[s.idx[i] - 1].item); s.rawDel i; return true
  do: return false

proc pop*[A](s: var OLSet[A]): var A {.inline.} =
  if s.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  let i = s.rawGetLast                  # Find idx[] pointing to last element
  result = move(s.data[^1])             # seq pop avoids O(len) shift on data
  s.rawDel i                            # does the s.data.pop

proc clear*[A](s: var OLSet[A]) =
  if s.idx.len == 0: return
  s.data.setLen 0
  zeroMem s.idx[0].addr, s.idx.len * s.idx[0].sizeof

iterator items*[A](s: OLSet[A]): A =
  let L = s.len
  for cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield cell.item

iterator mitems*[A](s: var OLSet[A]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i].item

iterator pairs*[A](s: OLSet[A]): tuple[a: int, b: A] =
  let L = s.len
  for i, cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield (i, s.data[i].item)

iterator hcodes*[A](s: OLSet[A]): tuple[i: int, hc: Hash] =
  let L = s.len
  for cell in s.data:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield cell.hcode

iterator allItems*[A](s: OLSet[A]; item: A): A =
  let L = s.len
  let hc0 = hash0(item)                #QUESTION: does equal(...,hc0) work here?
  for i in probeSeq(s.hashHc(hc0), s.idx.high):
    assert(s.len == L, "cannot change a set while iterating over it")
    if not isUsed(s.idx, i): break
    if s.equal(s.idx[i] - 1, item, hc0): yield s.data[i].item

proc debugDump*[A](s: OLSet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  echo "DATA:"
  for i, cell in s.data:
    echo "  i: ", i, " val: ", cell
  echo "IDX:"
  for i, ix in s.idx:
    echo "  i: ", i, " idx: ", ix

defSetOps(OLSet)    # Define rest of the wide API not depending on repr details
