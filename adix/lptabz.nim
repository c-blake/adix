## This module provides an (in|un)ordered multiset/multitable representation via
## Linear Probing with aging friendly Backshift Delete(Knuth TAOCPv3) & optional
## Robin Hood reorg (Celis,1986).  Linear probing collision clusters yields "no
## tuning needed" locality of reference - 1 DRAM hit per access for large tables
## of small items.  RH sorts collision clusters by search depth which adds nice
## properties: faster miss search (eg. for inserts, usually compensating for
## data motion) and min depth variance (no unlucky keys).  The latter enables
## ~100% table space utilization (we need one empty slot to stop some loops).
##
## Misuse/attack is always possible.  We provide several mitigations triggered,
## like table growth, by overlong scans (but on underfull tables): A) automatic
## rehash of user `hash` output with a strong hash, B) overlong scan warnings
## (disabled in ``danger`` mode), and C) automatic Robin Hood re-org activation.
## Defaults are to always rehash and use RobinHood re-org since this is safest,
## but all parameters here can be tuned per program and per instance.  This
## module uses `althash.getSalt` to allow override of the per-instance per-
## table space hash salt, allowing it to be as easy/hard to predict as desired.
##
## Multiset personality ensues when the ``V`` value type generic parameter is
## ``void``.  Otherwise the style of interface is multitable.  Every attempt is
## made for either to be drop-in compatible with Nim's standard library sets &
## tables, but extra features provided here.
##
## Space-time optimization of a sentinel key (a value of ``K`` disallowed for
## ordinary keys) is supported through the final two generic parameters, ``Z``,
## and ``z``.  If ``Z`` is `void`, hash codes are saved and ``z`` is ignored.
## If ``Z==K``, ``z`` is the sentinel key value.
##
## If ``Z`` is neither ``K`` nor ``void`` then compact, insertion-ordered mode
## is used and ``z`` means how many bits of hcode are saved beside an index into
## a dense ``seq[(K,V)]``.  6..8 bits avoids most "double cache misses" for miss
## lookups/inserts while 20..50 are needed to support ``hash`` call elision in
## ``setCap`` for large tables.  ``z=0`` works if space matters more than time.

import althash, memutil, bitop, heapqueue, sequint
export Hash, sequint
type                  ## `K` is Key type; `V` is Value type (can be `void`)
  HCell*[K,V,Z;z:static[int]] = object
    when Z is void:   ## void sentinel type => no sentinel; else `z` is sentinel
      hcode: int      #NOTE: ins-order mode hcodes, if any, are in idx[]
    key: K
    when V isnot void:
      val: V
  #XXX `seq`->`UncheckedArray` and add `save`, `loadLPTabz`, `mmapLPTabz` procs
  LPTabz*[K,V,Z;z:static[int]] = object           ## Robin Hood Hash Set
    when Z is K or Z is void:
      data: seq[HCell[K,V,Z,z]]                   # RobinHoodOptional LP HashTab
      count: int                                  # count of used slots
    else: # User set 3rd param to non-void, non-K; e.g. int8; => compact, z=bits
      data: seq[HCell[K,V,Z,z]]                   # data array; len == count
      idx: SeqUint                                # RobinHoodOptional LP HashTab
      hcBits: uint8                               # hcode bits to put in idx[]
    numer, denom, minFree, growPow2, pow2: uint8  # size policy parameters
    rehash, robin: bool                           # Steal 2-bits from `salt`?
    salt: Hash                                    # maybe unpredictable salt
  LPSetz*[K,Z;z:static[int]] = LPTabz[K,void,Z,z] ## LPTabz for sentinel Set
  LPTab*[K,V] = LPTabz[K,V,void,0]                ## LPTabz for no-sentinel Tab
  LPSet*[K] = LPTabz[K,void,void,0]               ## LPTabz for no-sentinel Set

var lpInitialSize* = 4 ## default initial size aka capacity aka cap
var lpNumer*       = 2 ## default numerator for lg(n) probe depth limit
var lpDenom*       = 1 ## default denominator for lg(n) probe depth limit
var lpMinFree*     = 1 ## default min free slots; (>= 1)
var lpGrowPow2*    = 1 ## default growth power of 2; 1 means double
var lpRehash*      = false ## default hcode rehashing behavior; auto-activated
var lpRobinHood*   = true ## default to Robin Hood re-org on insert/delete

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

proc len*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.count
  else:
    t.data.len

proc high[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.data.high
  else:
    t.idx.high

proc key[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], i: int): K {.inline.} =
  when Z is K or Z is void:
    t.data[i].key
  else:
    t.data[t.idx[i] - 1].key

proc getCap*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.data.len
  else:
    t.idx.len

proc isUsed[K,V,Z;z:static[int]](cell: HCell[K,V,Z,z]): bool {.inline.} =
  when Z is void:
    cell.hcode != 0
  else:
    cell.key != z

proc isUsed[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], i: int): bool {.inline.} =
  when Z is K or Z is void:
    t.data[i].isUsed
  else:
    t.idx[i] != 0

proc unUse[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], i: int,
                                clear=true) {.inline.} =
  when Z is K or Z is void:
    when Z is void:
      t.data[i].hcode = 0
      when compiles(default(t.data[i].val)):
        if clear:
          t.data[i].key = default(t.data[i].key)
    else:
      t.data[i].key = z
    when V isnot void and compiles(default(t.data[i].val)):
      if clear:
        t.data[i].val = default(t.data[i].val)
  else:
    t.idx[i] = 0

proc equal[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; i: int, key: K, hc: Hash):
    bool {.inline.} =
  when Z is K or Z is void:
    when type(key) is SomeInteger or Z isnot void:
      t.data[i].key == key
    else: # Compare hc 1st so missing => ~0 key cmp; Present => ~1 key cmp
      t.data[i].hcode == hc and t.data[i].key == key
  else:
    if z > 0: #XXX extract hcode from idx, etc.
      t.data[t.idx[i] - 1].key == key
    else:
      t.data[t.idx[i] - 1].key == key

proc pushUp[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], i, n: int) {.inline.} =
  when Z is K or Z is void:
    t.data.pushUp i, n
  else:
    for j in countdown(i + n - 1, i): t.idx[j+1] = t.idx[j]

proc set1[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i, j: int) {.inline.} =
  when Z is K or Z is void:
    t.data[i] = t.data[j]
  else:
    t.idx[i] = t.idx[j]

proc pullDown[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], i, n: int){.inline.}=
  when Z is K or Z is void:
    t.data.pullDown i, n
  else:
    for j in countup(i, i + n - 1): t.idx[j] = t.idx[j+1]

proc cell[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z],
                               i: int): ptr HCell[K,V,Z,z] {.inline.} =
  when Z is K or Z is void:
    t.data[i].unsafeAddr
  else:
    t.data[t.idx[i] - 1].unsafeAddr


proc hashHc[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; hc: Hash): Hash {.inline.}=
  if t.rehash: hash(hc, t.salt) else: hc

proc hash[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; i: int): Hash {.inline.} =
  when Z is void:
    t.hashHc t.data[i].hcode
  else:
    t.hashHc hash(t.key(i))

proc hash0[K,Z](key: K): Hash {.inline.} =
  result = hash(key)
  when Z is void:         # Account for hash==0, FREE marker
    if result == 0:       # Rarely taken branch very predictable
      result = 314159265  # Value matters little; More bits spread while growing

proc depth(i, hc, mask: Hash): Hash {.inline.} =
  let i = uint(i)
  let hc = uint(hc)
  let mask = uint(mask)
  Hash((i - hc) and mask)                 # Search depth of entry w/hcode @ix`i`

iterator probeSeq(hc, mask: Hash): int =  # WTF generic over K caused codegenbug
  var i: Hash = hc and mask               # Start w/home address
  while true:
    yield i
    i = (i + 1) and mask                  # Linear Probing

proc rawGet[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K;
                                 hc, d: var Hash): int {.inline.} =
  assert(t.getCap > 0, "Uninitialized LPTabz")  # Ensure in *caller* not here
  hc = hash0[K,Z](key)
  var j {.noInit.}: int                         # Where to insert if missing
  for i in probeSeq(t.hashHc(hc), t.high):
    j = i
    if not t.isUsed(i):                   # Need >=1 FREE slot to terminate
      break
    if t.robin and d > depth(i, t.hash(i), t.high):
      break
    if t.equal(i, key, hc):
      return i
    d.inc
    ifStats lpDepth.inc
  result = -1 - j               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K;
                                     hc, d: var Hash): int {.inline.} =
  assert(t.getCap > 0, "Uninitialized LPTabz") # Ensure in *caller* not here
  if d == 1:
    d = 0
  else:
    hc = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc), t.high):
    result = i
    if not t.isUsed(i):                # Need >=1 FREE slot to terminate
      break
    if t.robin and d > depth(i, t.hash(i), t.high):
      break
    d.inc
    ifStats lpDepth.inc

proc rawGet[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): int {.inline.} =
  var hc, d: Hash
  rawGet(t, key, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc rawGetLast[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]): int {.inline,used.} =
  let hc = hash0[K,Z](s.data[^1].key) # Can't get out of idx without a back-link
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    if s.idx[i].int == s.data.len: return i

proc depth[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): int {.inline.} =
  var hc, d: Hash
  discard rawGet(t, key, hc, d)
  d

proc depths*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): seq[int] =
  ## Compute & return exact distribution of search depths over a set
  for k in t.keys:
    let d = t.depth(k)
    if d >= result.len: result.setLen(d + 1)
    result[d] += 1

proc tooFull[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; d: int;
                                  newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if t.getCap - t.len < t.minFree.int + 1:
    dbg echo("Too little space (", t.getCap - t.len, ") of ", t.getCap)
    ifStats lpTooFull.inc
    newSize = t.getCap shl t.growPow2
    return
  if t.denom.int * (d - 1) < t.numer.int * t.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",t.pow2," depths: ",t.depths)
  ifStats lpTooDeep.inc
  if t.len > t.getCap shr t.growPow2:
    newSize = t.getCap shl t.growPow2
  else:
    dbg echo("Too sparse to grow, ", t.len, "/", t.getCap, " depth: ", d)
    ifStats lpTooSparse.inc     # Normal resizing cannot restore performance
    newSize = t.getCap
    var ext: string             # Extra text after primary message
    if t.rehash:                # Already re-hashing hash() output
      if t.robin:               # Already doing Robin Hood re-org
        ext = "; Switch to tree|seq for dups"
        result = false          # Could potentially auto-convert to B-tree here
      else:
        ext = "; Adapting by Robin Hood re-org"
        t.robin = true
    else:                       # Turn on re-hashing hash() output
      t.rehash = true
      ext = "; Adapting by re-hashing hash()"
    when defined(lpWarn) or not defined(danger):
      lpWarnCnt.inc
      if lpWarnCnt <= lpMaxWarn:
        lpWarn.write "LPTabz: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i: Hash; d: var int):
    int {.inline.} =
  if t.robin:
    result = i                          # Linear probe to first empty slot
    while t.isUsed(result):
      result = (result + 1) and t.high
      d.inc

proc rawPut2[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z];
                                  i, j: Hash): int {.inline.} =
  if t.robin:
    if j > i:                           # No table wrap around; just shift up
      t.pushUp i, j - i
    elif j < i:                         # j wrapped to low indices
      t.pushUp 0, j
      t.set1 0, t.high
      t.pushUp i, t.high - i
  # else:                               # j == i => already have space @i; done
  result = i

proc rawDel[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i: Hash) {.inline.} =
  when not (Z is K or Z is void):
    let j = t.idx[i] - 1                # To keep `data` density..
    if j.int + 1 < t.len:               # ..unless already points to data[^1]
      t.idx[t.rawGetLast] = j + 1       # ..retarget idx[data[^1]] -> j
      t.data[j] = t.data[^1]            # Copy last elt to `[j]`; move?
  let mask = t.high
  if t.robin:
    var k = i
    var j = (i + 1) and mask            # Find next empty|at home position entry
    while t.isUsed(j) and j != (t.hash(j) and mask):
      j = (j + 1) and mask
    if j > i + 1:                       # No table wrap around; just shift down
      t.pullDown i, j - 1 - i
      k = j - 1                         # Mark just-past-shift-block entry empty
    elif ((j + mask - i) and mask) > 0: # j wrapped to low indices; Did >0 j.inc
      t.pullDown i, mask - i
      t.set1 mask, 0
      t.pullDown 0, j - 1
      k = (j + mask) and mask           # [j-1 mod tabSz] is now empty
#   else:                               # k == i is already home position
    t.unUse k
  else:
    var i = i             # KnuthV3 Algo6.4R adapted for i=i+1 instead of i=i-1
    while true:           # The correctness of this depends on (i+1) in probeSeq
      var j = i           #..though may be adaptable to other simple sequences.
      t.unUse i                         # Mark current FREE
      while true:
        i = (i + 1) and mask            # Increment mod table size
        if not t.isUsed(i):             # End of collision cluster; All done
          return
        let h = t.hash(i) and mask      # "home" slot of key@i
        if not ((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
          break
      t.set1 j, i                       # data[i] will be marked FREE next loop
  when not (Z is K or Z is void):
    discard t.data.pop

template getPut(present: untyped, missing: untyped) {.dirty.} =
  if t.getCap == 0: t.init
  var hc, d, newSize: Hash
  var i = t.rawGet(key, hc, d)
  if i < 0:
    var j = t.rawPut1(-1 - i, d)
    if t.tooFull(d, newSize):
      t.setCap newSize
      d = 0
      i = t.rawGet(key, hc, d)
      j = t.rawPut1(-1 - i, d)
    var k = t.rawPut2(-1 - i, j)        # Maybe allocate a slot
    when compiles(t.count):
      t.count.inc
    when compiles(t.data[k].hcode):
      t.data[k].hcode = hc
    when compiles(t.idx):
      t.idx[k] = t.data.len + 1
      t.data.setLen t.data.len + 1
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  var i: int
  if t.getCap == 0 or (i = t.rawGet(key); i) < 0:
    missing
  else:
    when compiles(t.count):
      t.count.dec
    present
    t.unUse i, false

proc slotsGuess(count: int, numer=lpNumer, denom=lpDenom, minFree=lpMinFree):
    int {.inline.} = ceilPow2(count + minFree) # Might have a great hash

proc init*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z];
    initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
    growPow2=lpGrowPow2, rehash=lpRehash, robinhood=lpRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize)
  t.data     = newSeq[HCell[K,V,Z,z]](initialSize)
  when Z is K:
    if z != K(0):
      for i in 0 ..< initialSize: t.unUse(i, false)
  when Z is K or Z is void:
    t.salt   = getSalt(t.data[0].addr)
    t.count  = 0
  else:
    t.idx    = initSeqUint(initialSize) #XXX add hcode space
    t.salt   = getSalt(t.idx.addr0)
    t.data.setLen 0                   #Should make above newSeq like newSeqOfCap
  t.numer    = numer.uint8
  t.denom    = denom.uint8
  t.minFree  = max(1, minFree).uint8  #Block user allowing inf loop possibility
  t.growPow2 = uint8(growPow2)
  t.pow2     = uint8(initialSize.lg)
  t.rehash   = rehash
  t.robin    = robinhood

proc initLPTabz*[K,V,Z;z:static[int]](initialSize=lpInitialSize, numer=lpNumer,
    denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
    robinhood=lpRobinHood): LPTabz[K,V,Z,z] {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; numer=lpNumer,
    denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
    robinhood=lpRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  t.numer    = numer
  t.denom    = denom
  t.minFree  = minFree
  t.growPow2 = growPow2
  t.rehash   = rehash
  t.robin    = robinhood

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc setCap*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; newSize = -1) =
  if t.getCap == 0: t.init
  var newSz: int
  if newSize < 0:
    newSz = t.getCap shl t.growPow2
    t.pow2 += t.growPow2
  else:
    newSz = max(newSize, slotsGuess(t.len, minFree=t.minFree.int))
    t.pow2 = uint8(newSz.lg)
  if newSz == t.getCap and newSize == -1:
    return
  dbg echo("RESIZE@ ", t.len, "/", t.getCap, " ", t.len.float/t.getCap.float)
  when Z is K or Z is void:
    var old = newSeq[HCell[K,V,Z,z]](newSz)
    if t.rehash: t.salt = getSalt(old[0].addr)
    swap(t.data, old)
    when Z is K:
      var hc: Hash
      if z != K(0):
        for i in 0 ..< t.getCap: t.unUse(i, false)
    for cell in old.mitems:
      if not cell.isUsed: continue
      when Z is K:
        var d: Hash = 0
        let j = t.rawGetDeep(cell.key, hc, d)
      else:
        var d: Hash = 1
        let j = t.rawGetDeep(cell.key, cell.hcode, d)
      t.cell(t.rawPut2(j, t.rawPut1(j, d)))[] = cell
  else:
    t.idx = initSeqUint(newSz)
    if t.rehash: t.salt = getSalt(t.idx.addr0)
    var hc: Hash              #XXX must loop over idx[]!=0 for hc's
    for i, cell in t.data:
      var d: Hash = 0
      let j = t.rawGetDeep(cell.key, hc, d)
      t.idx[t.rawPut2(j, t.rawPut1(j, d))] = i + 1
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", t.salt)

proc contains*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K):
    bool {.inline.} =
  t.getCap > 0 and t.rawGet(key) >= 0

proc containsOrIncl*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z];
                                        key: K): bool {.inline.} =
  getPut do: result = true
  do       : result = false; t.cell(k)[].key = key

proc setOrIncl*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z];
                                   key: K): bool {.inline.} =
  getPut do: t.cell(i)[].key = key; result = true
  do       : t.cell(k)[].key = key; result = false

proc mgetOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K;
                                     val: V): var V {.inline.} =
  getPut do: result = t.cell(i)[].val
  do       : (let c = t.cell(k); c[].key = key; c[].val = val; result = c[].val)

proc mgetOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K; val: V;
                                     had: var bool): var V {.inline.} =
  getPut do:
    had=true ; result = t.cell(i)[].val
  do:
    had=false; (let c = t.cell(k)); c[].key = key; c[].val = val
    result = c[].val

template editOrInit*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K;
                                          v, body1, body2: untyped) =
  getPut do:
    var v {.inject.} = t.cell(i)[].val.addr
    body1
  do:
    let c = t.cell(k)
    c[].key = key
    var v {.inject.} = c[].val.addr
    body2

template doAdd(body: untyped) {.dirty.} =
  if t.getCap == 0: t.init
  var hc, d, newSize: Hash
  var i = t.rawGetDeep(key, hc, d)
  var j = t.rawPut1(i, d)
  if t.tooFull(d, newSize):
    t.setCap newSize
    d = 0
    i = t.rawGetDeep(key, hc, d)
    j = t.rawPut1(i, d)
  let k = t.rawPut2(i, j)               # Maybe allocate a slot
  when compiles(t.data[k].hcode):
    t.data[k].hcode = hc
  when Z is K or Z is void:
    t.count.inc
  when compiles(t.idx):
    t.idx[i] = t.data.len + 1
    t.data.setLen t.data.len + 1
  t.cell(k)[].key = key
  body

proc add*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]; key: K) {.inline.} =
  doAdd: discard

proc add*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K,
                               val: V) {.inline.} =
  doAdd: t.cell(k)[].val = val

proc missingOrExcl*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K): bool =
  popRet do: t.rawDel i
  do: result = true

proc take*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z];
                              key: var K): bool {.inline.} =
  popRet do: key = move(t.cell(i)[].key); t.rawDel i; result = true
  do: discard

proc take*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K;
                                          val: var V): bool {.inline.} =
  popRet do:
    val = move(t.cell(i)[].val)
    t.rawDel i
    result = true
  do: discard

proc pop*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]): K {.inline.} =
  if t.getCap == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  when Z is K or Z is void:
    for e in t:       # Cheaper than it may look due to early exit, BUT can get
      result = e      #..slow as a set empties out.  Could keep running "min ix"
      t.excl result   #..based on t.flag (cheaply updated by either ins/del.
      return          #..Also broken if dups are present.  Performance XXX
  else:
    result = t.data[^1].key   # seq pop avoids O(len) shift on data
    t.rawDel t.rawGetLast     # does the s.data.pop

proc pop*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]):
    (K, V) {.inline.} =
  if t.getCap == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  for k,v in t:
    result[0] = k
    result[1] = v
    discard t.missingOrExcl(k)
    return            

proc clear*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]) =
  if t.getCap == 0: return
  when Z is K or Z is void:
    t.count = 0
    when Z is void:
      zeroMem t.data[0].addr, t.getCap * t.data[0].sizeof
    else:
      if z == K(0):
        zeroMem t.data[0].addr, t.getCap * t.data[0].sizeof
      else:
        for i in 0 ..< t.getCap: t.unUse i
  else:
    t.idx.clear
    t.data.setLen 0

iterator cells[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]): HCell[K,V,Z,z] =
  let L = s.len
  for cell in s.data:
    assert(s.len == L, "cannot edit while iterating")
    when Z is K or Z is void:
      if not cell.isUsed: continue
    yield cell

iterator mcells[K,V,Z;z:static[int]](s: var LPTabz[K,V,Z,z]):ptr HCell[K,V,Z,z]=
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot edit while iterating")
    when Z is K or Z is void:
      if not s.isUsed(i): continue
    yield s.data[i].addr

iterator items*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): K =
  for cell in s.cells: yield cell.key

iterator mitems*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z]): var K =
  for cell in s.mcells: yield cell.key

iterator hcodes*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): (int, Hash) =
  when Z is void:
    for cell in s.cells: yield (i, cell.hcode)
  else:
    for i, cell in 0 ..< s.getCap:
      if s.isUsed(i): yield (i, s.hash(i))

iterator allItems*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]; key: K): K =
  let L = s.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(s.hashHc(hc0), s.high):
    assert(s.len == L, "cannot edit while iterating")
    if not s.isUsed(i): break
    if s.equal(i, key, hc0): yield s.key(i)

iterator pairs*[K,Z;z:static[int]](t: LPTabz[K,void,Z,z]): (int, K) =
  var j = 0
  for cell in t.cells: yield (j, cell.key); j.inc

iterator pairs*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): (K,V) =
  for cell in t.cells: yield (cell.key, cell.val)

iterator mpairs*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]): (K, var V) =
  for cell in t.mcells: yield (cell.key, cell.val)

iterator keys*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): K =
  for cell in t.cells: yield cell.key

iterator values*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): V =
  for cell in t.cells: yield cell.val

iterator mvalues*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]): var V =
  for cell in t.mcells: yield cell.val

iterator allValues*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): V =
  let L = t.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc0), t.high):
    assert(t.len == L, "cannot change a tab while iterating over it")
    if not t.isUsed(i): break
    if t.equal(i, key, hc0): yield t.cell(i)[].val

proc debugDump*[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]; label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ",
         if s.isUsed(i): depth(i, s.hash(i), s.high) else: 0,
         " ", cell
  when compiles(s.idx):
    for i, j in s.idx:
      echo "  i: ", i, " depth: ",
           (if j == 0: 0 else: depth(i, s.hash(i), s.idx.high)), " idx: ", j

proc pop*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z];
                             key: var K): bool {.inline.} =
  s.take key

proc incl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.containsOrIncl(elt)

proc excl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.missingOrExcl(elt)

proc incl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z],
                              other: LPTabz[K,void,Z,z]) =
  for elt in other: s.incl elt

proc excl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z],
                              other: LPTabz[K,void,Z,z]) =
  for elt in other: s.excl elt

proc card*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): int {.inline.} = s.len

proc union*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result = s1
  result.incl s2

proc intersection*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result.init min(s1.len, s2.len)
  for elt in s1:
    if elt in s2: result.incl elt

proc difference*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result.init
  for elt in s1:
    if elt notin s2: result.incl elt

proc symmetricDifference*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result = s1
  for item in s2:
    if result.containsOrIncl(item): result.excl item

proc `+`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] {.inline.} =
  s1.union s2
proc `*`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] {.inline.} =
  s1.intersection s2
proc `-`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] {.inline.} =
  s1.difference s2
proc `-+-`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] {.inline.} =
  s1.symmetricDifference s2

proc disjoint*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): bool =
  for item in s1:
    if item in s2: return
  result = true

#XXX ALGO BUG: As written DOES NOT ACCOUNT FOR DUP KV pairs.
proc `<=`*[K,Z;z:static[int]](s, t: LPTabz[K,void,Z,z]): bool =
  if s.counter <= t.counter:
    for item in s:
      if item notin t: return
    result = true

proc `<`*[K,Z;z:static[int]](s, t: LPTabz[K,void,Z,z]): bool =
  s.counter != t.counter and s <= t

proc `==`*[K,Z;z:static[int]](s, t: LPTabz[K,void,Z,z]): bool =
  s.counter == t.counter and s <= t

proc map*[K,A,Z;z:static[int]](data: LPTabz[K,void,Z,z], op: proc (x: K):
    A {.closure.}): LPTabz[A,void,Z,z] =
  result.init data.len
  for item in data: result.incl op(item)

proc toLPTabz*[K,Z;z:static[int]](keys: openArray[K],
                                  dups=false): LPTabz[K,void,Z,z] =
  result.init keys.len
  if dups:
    for item in keys: result.add item   # append; multi-set
  else:
    for item in keys: result.incl item  # replace; unique set

proc `$`*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): string =
  if s.len == 0: return "{}"
  result = "{"
  for key in s:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
  result.add("}")

proc hash*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): Hash =
  for i, hc in 0 .. s.hcodes: result = result xor hc
  result = !$result  #Important to use a COMMUTATIVE combiner above

proc depthStats*[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]):
    tuple[m1, m2: float; mx: int] = # non-central!
  ## Performance Forensics
  result.m1 = 0.0
  result.m2 = 0.0
  var norm = 0.0
  let ds = s.depths
  for i, d in ds:
    result.m1 += i.float * d.float
    result.m2 += i.float * i.float * d.float
    norm += d.float
  norm = 1.0 / norm
  result.m1 *= norm
  result.m2 *= norm
  result.mx = ds.len

proc toLPTabz*[K,V: not void,Z;z:static[int]](pairs: openArray[(K,V)],
                                              dups=false): LPTabz[K,V,Z,z] =
  result.init pairs.len
  if dups:
    for key, val in pairs: result.add(key, val) # append; multi-table
  else:
    for key, val in pairs: result[key] = val    # replace

proc `$`*[K,V: not void,Z;z:static[int]](t: LPTabz[K,V,Z,z]): string =
  if t.len == 0: return "{:}"
  result = "{"
  for key, val in t:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
    result.add(": ")
    result.addQuoted(val)
  result.add("}")

proc pop*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K;
                                         val: var V): bool {.inline.} =
  t.take key, val

template withValue*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K;
                                         value, body: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    body

template withValue*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K; value,
                                         body1, body2: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    body1
  else: body2

proc `[]`*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: raiseNotFound(key)

proc `[]`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z],
                                key: K): var V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: raiseNotFound(key)

proc `[]=`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z],
                                 key: K, val: V) {.inline.} =
  getPut do: t.cell(i)[].val = val # Replace FIRST FOUND item in multimap|insert
  do       : (let c = t.cell(k); c[].key = key; c[].val = val)

proc `{}`*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z],
                                key: K): V {.inline.} = t[key]
proc `{}`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z],
                                key: K): var V {.inline.} = t[key]
proc `{}=`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K;
                                 val: V) {.inline.} = t[key] = val

proc hasKey*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z],
                                  key: K): bool {.inline.} = key in t

proc hasKeyOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z],
                                       key: K, val: V): bool {.inline.} =
  discard t.mgetOrPut(key, val, result)

proc getOrDefault*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K,
                                        default=default(V)): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  result = if i >= 0: t.cell(i)[].val else: default

proc del*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K) {.inline.} =
  if t.missingOrExcl(key): key.raiseNotFound

proc del*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K,
                               had: var bool) {.inline.} =
  had = not t.missingOrExcl(key)

#XXX ALGO BUG: As written DOES NOT ACCOUNT FOR DUP KV pairs.  Must lift into
#    the low-level impl and do custom-rawGetDeep like loop.
proc `==`*[K,V,Z;z:static[int]](x, y: LPTabz[K,V,Z,z]): bool =
  if isNil(x): return isNil(y)            # 2 nil => true
  if isNil(y): return false               # 1 nil => false
  if x.counter != y.counter: return false # diff size => false
  for key, val in x:                      # diff insert orders => diff `data`
    if not y.hasKey(key) or y.getOrDefault(key) != val: return false
  return true

proc indexBy*[A, K,V,Z;z:static[int]](collection: A,
                                      index: proc(x: V): K): LPTabz[K,V,Z,z] =
  result.init
  for item in collection: result[index(item)] = item

#A few things to maybe totally obviate CountTable or let it be a type alias
proc inc*[K,V: SomeInteger,Z;z:static[int]](c: var LPTabz[K,V,Z,z], key: K,
                                            amount: SomeInteger=1) {.inline.} =
  c.mgetOrPut(key, 0).inc amount

proc merge*[K,V: SomeInteger,Z;z:static[int]](c: var LPTabz[K,V,Z,z],
                                              b: LPTabz[K,V,Z,z]) =
  for key, val in b: c.inc(key, val)

iterator topPairs*[K,V:SomeInteger,Z;z:static[int]](c: LPTabz[K,V,Z,z], n=10,
                                                    min=V.low): (K,V) =
  var q = initHeapQueue[(V,K)]()
  for key, val in c:
    if val >= min:
      let e = (val, key)
      if q.len < n: q.push(e)
      elif e > q[0]: discard q.replace(e)
  while q.len > 0:        # q now has top n entries
    let r = q.pop
    yield (r[1], r[0])    # yield in ascending order

# Specializations;  Q: add ILSet/OLSet/etc. for back compat..?
proc initLPSetz*[K,Z;z:static[int]](initialSize=lpInitialSize, numer=lpNumer,
        denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
        robinhood=lpRobinHood): LPSetz[K,Z,z] {.inline.} =
  ## Return an LPTabz specialized to sets with a sentinel key.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc initLPSet*[K](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood): LPSet[K] {.inline.} =
  ## Return an LPTabz specialized to sets with no sentinel key.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc initLPTab*[K,V](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                     minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                     robinhood=lpRobinHood): LPTab[K,V] {.inline.} =
  ## Return an LPTabz specialized to tables with no sentinel key.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
