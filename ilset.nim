## This module provides a Robin Hood with Linear Probing hash set like ``lpset``
## specialized to integer keys of any width that possess a convenient sentinel
## specified at instantiation time as a generic type parameter.  See ``lpset``
## documentation for more details.  Unlike stdlib ``intsets``, satellite data is
## allowed so these sets can also provide fast tables.  Memory use is small,
## just the size of the stored keyed rows (e.g. as little as 2 bytes per entry)
## at 100% utilization.  Realized utilization depends upon hash quality relative
## to key sets.  Small memory usage is achieved by recomputing hashes on demand
## which for integers is only marginally slower than saving them (with a fast
## Fibonacci-esque hash, anyway).

import tabkey, althash, memutil, bitop, setopz
type                  # `A` is Element type; Nim leans toward `item` language.
  ILSet*[A; z: static[int]] = object  ## RobinHoodLP w/sentinel'd int keys
    data: seq[A]                # data array; CAUTION!
    count: int                  # count of used slots
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

when defined(hashStats):    # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x   # all kind of like "times" - you v0=val;...; val-v0
  var ilDepth* = 0      ## Counts total search depth
  var ilTooFull* = 0    ## Counts resizes from minFree boundary
  var ilTooDeep* = 0    ## Counts resizes from deep probe sequences
  var ilTooSparse* = 0  ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(ilWarn) or not defined(danger):
  var ilWarn* = stderr  ## Set to wherever you want warnings to go
  var ilMaxWarn* = 10   ## Most warnings per program invocation
  var ilWarnCnt = 0     # Running counter of warnings issued

proc isUsed[A; z: static[int]](s: ILSet[A,z], data: seq[A],
                               i: int): bool {.inline.} =
  data[i].getKey != z

proc hash0[A; z: static[int]](s: ILSet[A,z], item: A): Hash {.inline.} =
  assert z != 314159265, "illegal sentinel 314159265"
  result = hash(item)   # Account for hash() returning the sentinel.
  if result == z:       # Rarely taken branch very predictable
    result = 314159265  # Value matters little; More bits spread while enlarging

# s.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[A; z: static[int]](s: ILSet[A,z], hc: Hash): Hash {.inline.} =
  if s.rehash: hash(hc, s.salt) else: hc

proc hash[A; z: static[int]](s: ILSet[A,z], i: int): Hash {.inline.} =
  s.hashHc s.hash0(s.data[i])

proc equal[A; z: static[int]](s: ILSet[A,z], i: int, item: A,
                              hc: Hash): bool {.inline.} =
  when compiles(item.getKey) and type(item.getKey) is SomeInteger:
    s.data[i].getKey == item.getKey
  elif type(item) is SomeInteger:
    s.data[i] == item

proc depth(i, hc, mask: Hash): Hash {.inline.} =
  (i - hc) and mask                       # Search depth of entry w/hcode @ix`i`

iterator probeSeq(hc, mask: Hash): int =  # WTF generic over A caused codegenbug
  var i: Hash = hc and mask               # Start w/home address
  while true:
    yield i
    i = (i + 1) and mask                  # Linear Probing

proc rawGet[A; z: static[int]](s: ILSet[A,z]; item: A;
                               hc, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized ILSet") # Ensure in *caller* not here
  hc = s.hash0(item)
  var t {.noInit.}: int                         # Where to insert if missing
  for i in probeSeq(s.hashHc(hc), s.data.high):
    t = i
    if not s.isUsed(s.data, i):                # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.data.high):
      break
    if s.equal(i, item, hc):
      return i
    d.inc
    ifStats ilDepth.inc
  result = -1 - t               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep[A; z: static[int]](s: ILSet[A,z]; item: A;
                                   hc, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized ILSet") # Ensure in *caller* not here
  hc = s.hash0(item)
  for i in probeSeq(s.hashHc(hc), s.data.high):
    result = i
    if not s.isUsed(s.data, i):                # Need >=1 FREE slot to terminate
      break
    if s.robin and d > depth(i, s.hash(i), s.data.high):
      break
    d.inc
    ifStats ilDepth.inc

proc rawGet[A; z: static[int]](s: ILSet[A,z], item: A): int {.inline.} =
  var hc, d: Hash
  rawGet(s, item, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc depth[A; z: static[int]](s: ILSet[A,z]; item: A): int {.inline.} =
  var hc, d: Hash
  discard rawGet(s, item, hc, d)
  d

proc tooFull[A; z: static[int]](s: var ILSet[A,z]; d: int;
                                newSize: var int): bool {.inline.} =
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
    var ext: string             # extra text after primary message
    if s.robin:                 # Robin Hood already active
      if s.rehash:              # rehashing hash() already active
        ext = "; Switch to tree|seq for dups"
        result = false          # Could potentially auto-convert to B-tree here
        newSize = s.data.len    #XXX Try 2nd kind of rehash|maybe a `hash2()`?
      else:
        ext = "; Adapting by re-hashing hash()"
        s.rehash = true
        newSize = s.data.len
    else:                       # Turn on Robin Hood
      s.robin = true
      ext = "; Adapting by Robin Hood re-org"
    when defined(ilWarn) or not defined(danger):
      ilWarnCnt.inc
      if ilWarnCnt <= ilMaxWarn:
        ilWarn.write "ILSet: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1[A; z: static[int]](s: var ILSet[A,z], i: Hash;
                                d: var int): int {.inline.} =
  if s.robin:
    result = i                          # Linear probe to first empty slot
    while s.isUsed(s.data, result):
      result = (result + 1) and s.data.high
      d.inc

proc rawPut2[A; z: static[int]](s: var ILSet[A,z], i, j: Hash): int {.inline.} =
  if s.robin:
    if j > i:                           # No table wrap around; just shift up
      pushUp s.data, i, j - i
    elif j < i:                         # j wrapped to low indices
      pushUp s.data, 0, j
      s.data[0] = s.data[s.data.high]
      pushUp s.data, i, s.data.high - i
# else:                                 # j == i => already have space @i; done
  result = i

proc rawDel[A; z: static[int]](s: var ILSet[A,z], i: Hash) {.inline.} =
  let mask = s.data.high
  if s.robin:
    var k = i
    var j = (i + 1) and mask            # Find next empty|at home position entry
    while s.isUsed(s.data, j) and j != (s.hash(j) and mask):
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
    s.data[k] = default(type(s.data[k]))
    s.data[k].setKey z
  else:
    var i = i             # KnuthV3 Algo6.4R adapted for i=i+1 instead of i=i-1
    while true:           # The correctness of this depends on (i+1) in probeSeq
      var j = i           #..though may be adaptable to other simple sequences.
      s.data[i] = default(type(s.data[i]))  # Mark current FREE
      s.data[i].setKey z             
      while true:
        i = (i + 1) and mask            # ILcrement mod table size
        if not s.isUsed(s.data, i):     # End of collision cluster; All done
          return
        let h = s.hash(i) and mask      # "home" slot of key@i
        if not ((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
          break
      s.data[j] = move(s.data[i])       # data[i] will be marked FREE next loop

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
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0:
    missing
  let i = s.rawGet(item)
  if i >= 0:
    s.data[i].setKey z
    s.count.dec
    present
  else:
    missing

var ilInitialSize* = 4 ## default initial size aka capacity aka cap
var ilNumer*       = 3 ## default numerator for lg(n) probe depth limit
var ilDenom*       = 2 ## default denominator for lg(n) probe depth limit
var ilMinFree*     = 1 ## default min free slots; (>= 1)
var ilGrowPow2*    = 1 ## default growth power of 2; 1 means double
var ilRehash*      = true ## default hcode rehashing behavior; disable@own peril
var ilRobinHood*   = true ## default to Robin Hood re-org on insert/delete

proc init*[A; z: static[int]](s: var ILSet[A,z], initialSize=ilInitialSize,
                              numer=ilNumer, denom=ilDenom, minFree=ilMinFree,
                              growPow2=ilGrowPow2, rehash=ilRehash,
                              robinhood=ilRobinHood) {.inline.} =
  s.data     = newSeq[A](initialSize)
  if z != 0:
    for i in 0..<initialSize: s.data[i].setKey z
  s.salt     = getSalt(s.data[0].addr)
  s.numer    = numer.uint8
  s.denom    = denom.uint8
  s.minFree  = max(1, minFree).uint8 # Block user allowing inf loop possibility
  s.growPow2 = uint8(growPow2)
  s.pow2     = uint8(initialSize.lg)
  s.rehash   = rehash
  s.robin    = robinhood
  s.count    = 0

proc initILSet*[A; z: static[int]](initialSize=ilInitialSize, numer=ilNumer,
                                   denom=ilDenom, minFree=ilMinFree,
                                   growPow2=ilGrowPow2, rehash=ilRehash,
                                   robinhood=ilRobinHood): ILSet[A,z]{.inline.}=
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[A; z: static[int]](s: var ILSet[A,z], numer=ilNumer,
                                   denom=ilDenom, minFree=ilMinFree,
                                   growPow2=ilGrowPow2, rehash=ilRehash,
                                   robinhood=ilRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  s.numer    = numer
  s.denom    = denom
  s.minFree  = minFree
  s.growPow2 = growPow2
  s.rehash   = rehash
  s.robin    = robinhood

proc rightSize*(count: int, numer=ilNumer, denom=ilDenom, minFree=ilMinFree):
    int {.inline.} = ceilPow2(count + minFree) # Might have a great hash

proc len*[A; z: static[int]](s: ILSet[A,z]): int {.inline.} = s.count

proc getCap*[A; z: static[int]](s: ILSet[A,z]): int {.inline.} = s.data.len

proc setCap*[A; z: static[int]](s: var ILSet[A,z], newSize = -1) =
  if s.data.len == 0: s.init
  var newSz: int
  if newSize < 0:
    newSz = s.data.len shl s.growPow2
    s.pow2 += s.growPow2
  else:
    newSz = max(newSize, rightSize(s.count, minFree=s.minFree.int))
    s.pow2 = uint8(newSz.lg)
  if newSz == s.data.len and newSize == -1:
    return
  dbg echo("RESIZE@ ",s.count,"/",s.data.len," ",s.count.float/s.data.len.float)
  var old: seq[A]
  var hc, d: Hash
  newSeq(old, newSz)
  if s.rehash: s.salt = getSalt(old[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", s.salt)
  swap(s.data, old)
  if z != 0:
    for i in 0 ..< s.data.len: s.data[i].setKey z
  for i in 0 ..< old.len:
    if s.isUsed(old, i):
      d = 0
      let j = s.rawGetDeep(old[i], hc, d)
      s.data[s.rawPut2(j, s.rawPut1(j, d))] = move(old[i])

proc contains*[A; z: static[int]](s: ILSet[A,z], item: A): bool {.inline.} =
  if s.data.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A; z: static[int]](s: var ILSet[A,z],
                                        item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k] = item

proc setOrIncl*[A; z: static[int]](s: var ILSet[A,z],
                                   item: A): bool {.inline.} =
  getPut do: s.data[i] = item; result = true
  do       : s.data[k] = item; result = false

proc mgetOrIncl*[A; z: static[int]](s: var ILSet[A,z], item: A,
                                    had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[i]
  do       : had = false; s.data[k] = item; return s.data[k]

proc add*[A; z: static[int]](s: var ILSet[A,z], item: A) {.inline.} =
  if s.data.len == 0: s.init
  var hc, d, newSize: Hash
  var i = s.rawGetDeep(item, hc, d)
  var j = s.rawPut1(i, d)
  if s.tooFull(d, newSize):
    s.setCap
    d = 0
    i = s.rawGetDeep(item, hc, d)
    j = s.rawPut1(i, d)
  let k = s.rawPut2(i, j)               # Maybe allocate a slot
  s.data[k] = item
  s.count.inc

template withItem*[A; z: static[int]](s: var ILSet[A,z], itm: A,
                                      it, body: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].addr
    body

template withItem*[A; z: static[int]](s: var ILSet[A,z], itm: A,
                                      it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].addr
    body1
  else:
    body2

template withItem*[A; z: static[int]](s: ILSet[A,z], itm: A,
                                      it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i]
    body1
  else:
    body2

proc missingOrExcl*[A; z: static[int]](s: var ILSet[A,z], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A; z: static[int]](s: var ILSet[A,z], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[i]); s.rawDel i; return true
  do: return false

proc pop*[A; z: static[int]](s: var ILSet[A,z]): var A {.inline.} =
  if s.data.len == 0: raise newException(ILdexError, formatErrorIndexBound(0,0))
  for e in s:         # Cheaper than it may look due to early exit, BUT can get
    result = move(e)  #..slow as a set empties out.  Could keep running "min ix"
    s.excl result     #..based on s.flag (cheaply updated by either ins/del.
    return            #..Also broken if dups are present. XXX

proc clear*[A; z: static[int]](s: var ILSet[A,z]) =
  if s.data.len == 0: return
  s.count = 0
  if z == 0:
    zeroMem s.data[0].addr, s.data.len * s.data[0].sizeof
  else:
    for i in 0..<s.data.len:
      s.data[i] = default(A)
      s.data[i].setKey z

iterator items*[A; z: static[int]](s: ILSet[A,z]): A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(s.data, i): yield s.data[i]

iterator mitems*[A; z: static[int]](s: var ILSet[A,z]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(s.data, i): yield s.data[i]

iterator pairs*[A; z: static[int]](s: ILSet[A,z]): tuple[a: int, b: A] =
  let L = s.len
  var j =  0
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(s.data, i): yield (j, s.data[i])
    j.inc

iterator hcodes*[A; z: static[int]](s: ILSet[A,z]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(s.data, i): yield (i, s.hash0(s.data[i]))

iterator allItems*[A; z: static[int]](s: ILSet[A,z]; item: A): A =
  let L = s.len
  let hc0 = hash0(item)                #QUESTION: does equal(...,hc0) work here?
  for i in probeSeq(s.hashHc(hc0), s.data.high):
    assert(s.len == L, "cannot change a set while iterating over it")
    if not s.isUsed(s.data, i): break
    if s.equal(i, item, hc0): yield s.data[i]

proc debugDump*[A; z: static[int]](s: ILSet[A,z], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ",
         if s.isUsed(s.data, i): depth(i, s.hash(i), s.data.high) else: 0,
         " ", cell

defSetOps(ILSet)    # Define rest of the wide API not depending on repr details
