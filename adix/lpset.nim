## This module implements an unordered set representation via linear probing
## with aging friendly Backshift Delete (Knuth TAOCPv3) and optional Robin Hood
## reorganization (Celis,1986).  Linear probing has cache oblivious/no tuning
## needed locality - 1 DRAM hit per access followed by a short linear scan.  RH
## keeps cluster items sorted by search depth.  RH re-org adds nice properties:
## faster missing search (eg. for inserts, usually more than compensating for
## data motion) and min over-table variance (no unlucky keys).  Small variance
## gives (some) resilience to weak hashes and very high utilization of table
## space.  It is not crazy to run a Robin Hood table at 100% full (though here
## we require 1 empty slot to terminate certain loops).
##
## Misuse/attack is always possible.  So, we provide an early warning system,
## disabled in ``danger`` mode, for overlong scans on underfull tables.  We also
## allow "re-hashing" the output of hash() to mix bits more in case that helps
## and switch to that mode automatically on overlong probe sequences.  Defaults
## are to always rehash and use RobinHood re-org since this is safest, but with
## all parameters here a user can both change defaults as well as tune on a
## per-instance basis.
##
## With Robin Hood, performance responds more to your hash() and to rehash=true
## than to ``numer, denom, minFree``.  Nevertheless, some very approximate lg(N)
## depth ratio advice: ``numer/denom`` for *random* (i.e. well hashed) data are
## are =~ RH(1/1) => ~71% data load & rmsDepth 2, RH(4/1) => ~73% data load,
## rmsDepth 2.1 while for vanilla LP (1/1) => ~37% and (4/1) => ~62%.  So, about
## 2..4/1 is probably most comparable to typical load-based resize rules, but if
## you have a near perfect hash you can profit greatly from that (100% 1 probe).

import althash, memutil, bitop, setops
type                  # `A` is Element type; Nim leans toward `item` language.
  HCell[A] = tuple[hcode: Hash, item: A]
  LPSet*[A] = object                          ## Robin Hood Hash Set
    data: seq[HCell[A]]                       # data array
    count: int                                # count of used slots
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

proc len*[A](s: LPSet[A]): int {.inline.} = s.count

proc isUsed[A](data: seq[HCell[A]], i: int): bool {.inline.} =
  data[i].hcode != 0

when defined(hashStats):    # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x   # all kind of like "times" - you v0=val;...; val-v0
  var lpDepth* = 0      ## Counts total search depth
  var lpTooFull* = 0    ## Counts resizes from minFree boundary
  var lpTooDeep* = 0    ## Counts resizes from deep probe sequences
  var lpTooSparse* = 0  ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(lpWarn) or not defined(danger):
  var lpWarn* = stderr  ## Set to wherever you want warnings to go
  var lpMaxWarn* = 10   ## Most warnings per program invocation
  var lpWarnCnt = 0     # Running counter of warnings issued

# s.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[A](s: LPSet[A], hc: Hash): Hash {.inline.} =
  if s.rehash: hash(hc, s.salt) else: hc

proc hash[A](s: LPSet[A], i: int): Hash {.inline.} =
  s.hashHc s.data[i].hcode

proc hash0[A](item: A): Hash {.inline.} =
  result = hash(item)   # Account for hash() returning zero, our FREE sentinel.
  if result == 0:       # Rarely taken branch very predictable
    result = 314159265  # Value matters little; More bits spread while enlarging

proc equal[A](s: LPSet[A], i: int, item: A, hc: Hash): bool {.inline.} =
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

proc rawGet[A](s: LPSet[A]; item: A; hc, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized LPSet") # Ensure in *caller* not here
  hc = hash0(item)
  var t {.noInit.}: int                         # Where to insert if missing
  for i in probeSeq(s.hashHc(hc), s.data.high):
    t = i
    if not isUsed(s.data, i):                # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.data.high):
      break
    if s.equal(i, item, hc):
      return i
    d.inc
    ifStats lpDepth.inc
  result = -1 - t               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep[A](s: LPSet[A]; item: A; hc, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized LPSet") # Ensure in *caller* not here
  hc = hash0(item)
  for i in probeSeq(s.hashHc(hc), s.data.high):
    result = i
    if not isUsed(s.data, i):                # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.data.high):
      break
    d.inc
    ifStats lpDepth.inc

proc rawGet[A](s: LPSet[A], item: A): int {.inline.} =
  var hc, d: Hash
  rawGet(s, item, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc depth[A](s: LPSet[A]; item: A): int {.inline.} =
  var hc, d: Hash
  discard rawGet(s, item, hc, d)
  d

proc tooFull[A](s: var LPSet[A]; d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if s.data.len - s.count < s.minFree.int + 1:
    dbg echo("Too little space (",s.data.len-s.count,") of ",s.data.len)
    ifStats lpTooFull.inc
    newSize = s.data.len shl s.growPow2
    return
  if s.denom.int * (d - 1) < s.numer.int * s.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",s.pow2," depths: ",s.depths)
  ifStats lpTooDeep.inc
  if s.count > s.data.len shr s.growPow2:
    newSize = s.data.len shl s.growPow2
  else:
    dbg echo("Too sparse to grow, ",s.count,"/",s.data.len," depth: ",d)
    ifStats lpTooSparse.inc     # Normal resizing cannot restore performance
    newSize = s.data.len
    var ext: string             # Extra text after primary message
    if s.rehash:                # Already re-hashing hash() output
      if s.robin:               # Already doing Robin Hood re-org
        ext = "; Switch to tree|seq for dups"
        result = false          # Could potentially auto-convert to B-tree here
      else:
        ext = "; Adapting by Robin Hood re-org"
        s.robin = true
    else:                       # Turn on re-hashing hash() output
      s.rehash = true
      ext = "; Adapting by re-hashing hash()"
    when defined(lpWarn) or not defined(danger):
      lpWarnCnt.inc
      if lpWarnCnt <= lpMaxWarn:
        lpWarn.write "LPSet: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1[A](s: var LPSet[A], i: Hash; d: var int): int {.inline.} =
  if s.robin:
    result = i                          # Linear probe to first empty slot
    while isUsed(s.data, result):
      result = (result + 1) and s.data.high
      d.inc

proc rawPut2[A](s: var LPSet[A]; i, j: Hash): int {.inline.} =
  if s.robin:
    if j > i:                           # No table wrap around; just shift up
      pushUp s.data, i, j - i
    elif j < i:                         # j wrapped to low indices
      pushUp s.data, 0, j
      s.data[0] = s.data[s.data.high]
      pushUp s.data, i, s.data.high - i
# else:                                 # j == i => already have space @i; done
  result = i

proc rawDel[A](s: var LPSet[A], i: Hash) {.inline.} =
  let mask = s.data.high
  if s.robin:
    var k = i
    var j = (i + 1) and mask            # Find next empty|at home position entry
    while isUsed(s.data, j) and j != (s.hash(j) and mask):
      j = (j + 1) and mask
    if j > i + 1:                       # No table wrap around; just shift down
      pullDown s.data, i, j - 1 - i
      k = j - 1                         # Mark just-past-shift-block entry empty
    elif ((j + mask - i) and mask) > 0: # j wrapped to low indices; Did >0 j.inc
      pullDown s.data, i, mask - i
      s.data[mask] = s.data[0]
      pullDown s.data, 0, j - 1
      k = (j + mask) and mask           # [j-1 mod tabSz] is now empty
#   else:                               # k == i is already home position
    s.data[k].hcode = 0
    s.data[k].item = default(type(s.data[k].item))
  else:
    var i = i             # KnuthV3 Algo6.4R adapted for i=i+1 instead of i=i-1
    while true:           # The correctness of this depends on (i+1) in probeSeq
      var j = i           #..though may be adaptable to other simple sequences.
      s.data[i].hcode = 0                   # Mark current FREE
      s.data[i].item = default(type(s.data[i].item))
      while true:
        i = (i + 1) and mask                # Increment mod table size
        if not isUsed(s.data, i):           # End of collision cluster; All done
          return
        let h = s.hash(i) and mask    # "home" slot of key@i
        if not ((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
          break
      s.data[j] = move(s.data[i])     # data[i] will be marked FREE next loop

# From here down is very similar for any open addr table (maybe w/getData(i)..)
# except for unkeyed-`pop` and `allItems` which do care about internal layout.
template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0: s.init
  var hc, d, newSize: Hash
  var i = s.rawGet(item, hc, d)
  if i < 0:
    var j = s.rawPut1(-1 - i, d)
    if s.tooFull(d, newSize):
      s.setCap newSize
      d = 0
      i = s.rawGet(item, hc, d)
      j = s.rawPut1(-1 - i, d)
    let k = s.rawPut2(-1 - i, j)        # Maybe allocate a slot
    s.count.inc
    s.data[k].hcode = hc
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0:
    missing
  let i = s.rawGet(item)
  if i >= 0:
    s.data[i].hcode = 0
    s.count.dec
    present
  else:
    missing

var lpInitialSize* = 4 ## default initial size aka capacity aka cap
var lpNumer*       = 2 ## default numerator for lg(n) probe depth limit
var lpDenom*       = 1 ## default denominator for lg(n) probe depth limit
var lpMinFree*     = 1 ## default min free slots; (>= 1)
var lpGrowPow2*    = 1 ## default growth power of 2; 1 means double
var lpRehash*      = false ## default hcode rehashing behavior; auto-activated
var lpRobinHood*   = true ## default to Robin Hood re-org on insert/delete

proc slotsGuess(count: int, numer=lpNumer, denom=lpDenom, minFree=lpMinFree):
    int {.inline.} = ceilPow2(count + minFree) # Might have a great hash

proc init*[A](s: var LPSet[A], initialSize=lpInitialSize, numer=lpNumer,
              denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2,
              rehash=lpRehash, robinhood=lpRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize)
  s.data     = newSeq[HCell[A]](initialSize)
  s.salt     = getSalt(s.data[0].addr)
  s.numer    = numer.uint8
  s.denom    = denom.uint8
  s.minFree  = max(1, minFree).uint8 # Block user allowing inf loop possibility
  s.growPow2 = uint8(growPow2)
  s.pow2     = uint8(initialSize.lg)
  s.rehash   = rehash
  s.robin    = robinhood
  s.count    = 0

proc initLPSet*[A](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood): LPSet[A] {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[A](s: var LPSet[A], numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  s.numer    = numer
  s.denom    = denom
  s.minFree  = minFree
  s.growPow2 = growPow2
  s.rehash   = rehash
  s.robin    = robinhood

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc getCap*[A](s: LPSet[A]): int {.inline.} = s.data.len

proc setCap*[A](s: var LPSet[A], newSize = -1) =
  if s.data.len == 0: s.init
  var newSz: int
  if newSize < 0:
    newSz = s.data.len shl s.growPow2
    s.pow2 += s.growPow2
  else:
    newSz = max(newSize, slotsGuess(s.count, minFree=s.minFree.int))
    s.pow2 = uint8(newSz.lg)
  if newSz == s.data.len and newSize == -1:
    return
  dbg echo("RESIZE@ ",s.count,"/",s.data.len," ",s.count.float/s.data.len.float)
  var old: seq[HCell[A]]
  var d: Hash
  newSeq(old, newSz)
  if s.rehash: s.salt = getSalt(old[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", s.salt)
  swap(s.data, old)
  for i in 0 ..< old.len:
    if isUsed(old, i):
      d = 0
      let j = s.rawGetDeep(old[i].item, old[i].hcode, d)
      s.data[s.rawPut2(j, s.rawPut1(j, d))] = move(old[i])

proc contains*[A](s: LPSet[A], item: A): bool {.inline.} =
  if s.data.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var LPSet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k].item = item

proc setOrIncl*[A](s: var LPSet[A], item: A): bool {.inline.} =
  getPut do: s.data[i].item = item; result = true
  do       : s.data[k].item = item; result = false

proc mgetOrIncl*[A](s: var LPSet[A], item: A, had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[i].item
  do       : had = false; s.data[k].item = item; return s.data[k].item

proc add*[A](s: var LPSet[A], item: A) {.inline.} =
  if s.data.len == 0: s.init
  var hc, d, newSize: Hash
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
  s.count.inc

template withItem*[A](s: var LPSet[A], itm: A; it,body1: untyped; body2: untyped=nil) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].item.addr
    body1
  else:
    body2

template withItem*[A](s: LPSet[A], itm: A; it,body1: untyped; body2: untyped=nil) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i].item.unsafeAddr
    body1
  else:
    body2

proc missingOrExcl*[A](s: var LPSet[A], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A](s: var LPSet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[i].item); s.rawDel i; return true
  do: return false

proc pop*[A](s: var LPSet[A]): var A {.inline.} =
  if s.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  for e in s:         # Cheaper than it may look due to early exit, BUT can get
    result = move(e)  #..slow as a set empties out.  Could keep running "min ix"
    s.excl result     #..based on s.flag (cheaply updated by either ins/del.
    return            #..Also broken if dups are present. XXX

proc clear*[A](s: var LPSet[A]) =
  if s.data.len == 0: return
  s.count = 0
  zeroMem s.data[0].addr, s.data.len * s.data[0].sizeof

iterator items*[A](s: LPSet[A]): A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield s.data[i].item

iterator mitems*[A](s: var LPSet[A]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield s.data[i].item

iterator pairs*[A](s: LPSet[A]): tuple[a: int, b: A] =
  let L = s.len
  var j = 0
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield (j, s.data[i].item)
    j.inc

iterator hcodes*[A](s: LPSet[A]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield (i, s.data[i].hcode)

iterator allItems*[A](s: LPSet[A]; item: A): A =
  let L = s.len
  let hc0 = hash0(item)                #QUESTION: does equal(...,hc0) work here?
  for i in probeSeq(s.hashHc(hc0), s.data.high):
    assert(s.len == L, "cannot change a set while iterating over it")
    if not isUsed(s.data, i): break
    if s.equal(i, item, hc0): yield s.data[i].item

proc debugDump*[A](s: LPSet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ",
         if isUsed(s.data, i): depth(i, s.hash(i), s.data.high) else: 0,
         " ", cell

defSetOps(LPSet)    # Define rest of the wide API not depending on repr details
