## This module provides an unordered multiset/multitable representation via
## linear probing with aging friendly Backshift Delete (Knuth TAOCPv3) and
## optional Robin Hood reorg (Celis,1986).  Linear probing collision clusters
## yields "no tuning needed" locality of reference - 1 DRAM hit per access for
## large tables of small items.  RH keeps clusters sorted by search depth which
## adds nice properties: faster miss searches (eg. for inserts, usually more
## than compensating for data motion) and min depth variance (no unlucky keys).
## The latter enables almost 100% table space utilization (though here we need
## an empty slot to terminate certain loops).
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

import althash, memutil, bitop, heapqueue
export Hash
type                  ## `K` is Key type; `V` is Value type (can be `void`)
  HCell*[K,V,Z;z:static[int]] = object
    when Z is void:   ## void sentinel type => no sentinel; else `z` is sentinel
      hcode: int
    key: K
    when V isnot void:
      val: V
  #XXX `seq`->`UncheckedArray` and add `save`, `loadLPTabz`, `mmapLPTabz` procs
  LPTabz*[K,V,Z;z:static[int]] = object                        ## Robin Hood Hash Set
    data: seq[HCell[K,V,Z,z]]                     # data array
    count: int                                # count of used slots
    salt: Hash
    numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool

proc len*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): int {.inline.} = t.count

proc isUsed[K,V,Z;z:static[int]](data: seq[HCell[K,V,Z,z]], i: int): bool {.inline.} =
  when Z is void:
    data[i].hcode != 0
  elif compiles(data[i].key.getKey):
    data[i].key.getKey != z
  else:
    data[i].key != z

proc unUse[K,V,Z;z:static[int]](data: var seq[HCell[K,V,Z,z]], i: int) {.inline.} =
  when Z is void:
    data[i].hcode = 0
  elif compiles(data[i].key.setKey):
    data[i].key.setKey z
  else:
    data[i].key = z

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

# t.salt here is just a hash of the VM address of data[] that can give distinct
# tabs distinct home addr locations at least as independent as `getSalt`.
proc hashHc[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; hc: Hash): Hash {.inline.} =
  if t.rehash: hash(hc, t.salt) else: hc

proc hash[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; i: int): Hash {.inline.} =
  when Z is void:
    t.hashHc t.data[i].hcode
  else:
    t.hashHc hash(t.data[i].key)

proc hash0[K,Z](key: K): Hash {.inline.} =
  result = hash(key)
  when Z is void:         # Account for hash==0, FREE marker
    if result == 0:       # Rarely taken branch very predictable
      result = 314159265  # Value matters little; More bits spread while growing

proc equal[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; i: int, key: K, hc: Hash): bool {.inline.} =
  when compiles(key.getKey):
    when type(key.getKey) is SomeInteger:
      t.data[i].key.getKey == key.getKey  # Do not waste time on `hc` cmp
    else:
      t.data[i].hcode == hc and t.data[i].key.getKey == key.getKey
  elif type(key) is SomeInteger:
    t.data[i].key == key
  else: # Prefix compare hc so missing => ~0 key cmp; Present => ~1 key cmp
    t.data[i].hcode == hc and t.data[i].key == key

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

proc rawGet[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K; hc, d: var Hash): int {.inline.} =
  assert(t.data.len > 0, "Uninitialized LPTabz") # Ensure in *caller* not here
  hc = hash0[K,Z](key)
  var j {.noInit.}: int                         # Where to insert if missing
  for i in probeSeq(t.hashHc(hc), t.data.high):
    j = i
    if not isUsed(t.data, i):                # Need >=1 FREE slot to terminate
      break
    if t.robin and d > depth(i, t.hash(i), t.data.high):
      break
    if t.equal(i, key, hc):
      return i
    d.inc
    ifStats lpDepth.inc
  result = -1 - j               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep[K,V,Z;z:static[int]](t:LPTabz[K,V,Z,z]; key: K; hc,d: var Hash): int{.inline.}=
  assert(t.data.len > 0, "Uninitialized LPTabz") # Ensure in *caller* not here
  if d == 1:
    d = 0
  else:
    hc = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc), t.data.high):
    result = i
    if not isUsed(t.data, i):                # Need >=1 FREE slot to terminate
      break
    if t.robin and d > depth(i, t.hash(i), t.data.high):
      break
    d.inc
    ifStats lpDepth.inc

proc rawGet[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): int {.inline.} =
  var hc, d: Hash
  rawGet(t, key, hc, d)        # < 0 => MISSING and insert idx = -1 - result

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

proc tooFull[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if t.data.len - t.count < t.minFree.int + 1:
    dbg echo("Too little space (",t.data.len-t.count,") of ",t.data.len)
    ifStats lpTooFull.inc
    newSize = t.data.len shl t.growPow2
    return
  if t.denom.int * (d - 1) < t.numer.int * t.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",t.pow2," depths: ",t.depths)
  ifStats lpTooDeep.inc
  if t.count > t.data.len shr t.growPow2:
    newSize = t.data.len shl t.growPow2
  else:
    dbg echo("Too sparse to grow, ",t.count,"/",t.data.len," depth: ",d)
    ifStats lpTooSparse.inc     # Normal resizing cannot restore performance
    newSize = t.data.len
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

proc rawPut1[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i: Hash; d: var int): int {.inline.} =
  if t.robin:
    result = i                          # Linear probe to first empty slot
    while isUsed(t.data, result):
      result = (result + 1) and t.data.high
      d.inc

proc rawPut2[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i, j: Hash): int {.inline.} =
  if t.robin:
    if j > i:                           # No table wrap around; just shift up
      pushUp t.data, i, j - i
    elif j < i:                         # j wrapped to low indices
      pushUp t.data, 0, j
      t.data[0] = t.data[t.data.high]
      pushUp t.data, i, t.data.high - i
  # else:                               # j == i => already have space @i; done
  result = i

proc rawDel[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; i: Hash) {.inline.} =
  let mask = t.data.high
  if t.robin:
    var k = i
    var j = (i + 1) and mask            # Find next empty|at home position entry
    while isUsed(t.data, j) and j != (t.hash(j) and mask):
      j = (j + 1) and mask
    if j > i + 1:                       # No table wrap around; just shift down
      pullDown t.data, i, j - 1 - i
      k = j - 1                         # Mark just-past-shift-block entry empty
    elif ((j + mask - i) and mask) > 0: # j wrapped to low indices; Did >0 j.inc
      pullDown t.data, i, mask - i
      t.data[mask] = t.data[0]
      pullDown t.data, 0, j - 1
      k = (j + mask) and mask           # [j-1 mod tabSz] is now empty
#   else:                               # k == i is already home position
    t.data[k].key = default(type(t.data[k].key))
    t.data.unUse k
  else:
    var i = i             # KnuthV3 Algo6.4R adapted for i=i+1 instead of i=i-1
    while true:           # The correctness of this depends on (i+1) in probeSeq
      var j = i           #..though may be adaptable to other simple sequences.
      t.data[i].key = default(type(t.data[i].key))
      t.data.unUse i                        # Mark current FREE
      while true:
        i = (i + 1) and mask                # Increment mod table size
        if not isUsed(t.data, i):           # End of collision cluster; All done
          return
        let h = t.hash(i) and mask    # "home" slot of key@i
        if not ((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
          break
      t.data[j] = move(t.data[i])     # data[i] will be marked FREE next loop

# From here down is very similar for any open addr table (maybe w/getData(i)..)
# except for unkeyed-`pop` and `allItems` which do care about internal layout.
template getPut(present: untyped, missing: untyped) {.dirty.} =
  if t.data.len == 0: t.init
  var hc, d, newSize: Hash
  var i = t.rawGet(key, hc, d)
  if i < 0:
    var j = t.rawPut1(-1 - i, d)
    if t.tooFull(d, newSize):
      t.setCap newSize
      d = 0
      i = t.rawGet(key, hc, d)
      j = t.rawPut1(-1 - i, d)
    let k = t.rawPut2(-1 - i, j)        # Maybe allocate a slot
    t.count.inc
    when compiles(t.data[k].hcode):
      t.data[k].hcode = hc
    missing
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if t.data.len == 0:
    missing
  let i = t.rawGet(key)
  if i >= 0:
    t.data.unUse i
    t.count.dec
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

proc init*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; initialSize=lpInitialSize, numer=lpNumer,
              denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2,
              rehash=lpRehash, robinhood=lpRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize)
  t.data     = newSeq[HCell[K,V,Z,z]](initialSize)
  when Z isnot void:
    if z != K(0):
      for i in 0 ..< initialSize: t.data.unUse(i)
  t.salt     = getSalt(t.data[0].addr)
  t.numer    = numer.uint8
  t.denom    = denom.uint8
  t.minFree  = max(1, minFree).uint8 # Block user allowing inf loop possibility
  t.growPow2 = uint8(growPow2)
  t.pow2     = uint8(initialSize.lg)
  t.rehash   = rehash
  t.robin    = robinhood
  t.count    = 0

proc initLPTabz*[K,V,Z;z:static[int]](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood): LPTabz[K,V,Z,z] {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
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

proc getCap*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): int {.inline.} = t.data.len

proc setCap*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; newSize = -1) =
  if t.data.len == 0: t.init
  var newSz: int
  if newSize < 0:
    newSz = t.data.len shl t.growPow2
    t.pow2 += t.growPow2
  else:
    newSz = max(newSize, slotsGuess(t.count, minFree=t.minFree.int))
    t.pow2 = uint8(newSz.lg)
  if newSz == t.data.len and newSize == -1:
    return
  dbg echo("RESIZE@ ",t.count,"/",t.data.len," ",t.count.float/t.data.len.float)
  var old: seq[HCell[K,V,Z,z]]
  newSeq(old, newSz)
  if t.rehash: t.salt = getSalt(old[0].addr)
  when defined(unstableHash):
    dbg echo(" NEW SALT: ", t.salt)
  swap(t.data, old)
  when Z isnot void:
    var hc: Hash
    if z != K(0):
      for i in 0 ..< t.data.len: t.data.unUse(i)
  for i in 0 ..< old.len:
    if isUsed(old, i):
      when Z isnot void:
        var d: Hash = 0
        let j = t.rawGetDeep(old[i].key, hc, d)
      else:
        var d: Hash = 1
        let j = t.rawGetDeep(old[i].key, old[i].hcode, d)
      t.data[t.rawPut2(j, t.rawPut1(j, d))] = move(old[i])

proc contains*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): bool {.inline.} =
  t.data.len > 0 and t.rawGet(key) >= 0

proc containsOrIncl*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]; key: K): bool {.inline.} =
  getPut do: result = true
  do       : result = false; t.data[k].key = key

proc setOrIncl*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]; key: K): bool {.inline.} =
  getPut do: t.data[i].key = key; result = true
  do       : t.data[k].key = key; result = false

proc mgetOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K;
                                     val: V): var V {.inline.} =
  getPut do: result = t.data[i].val
  do       : t.data[k].key = key; t.data[k].val = val; result = t.data[k].val

proc mgetOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K; val: V;
                                     had: var bool): var V {.inline.} =
  getPut do:
    had = true ; result = t.data[i].val
  do:
    had = false; t.data[k].key = key; t.data[k].val = val; result = t.data[k].val

template editOrInit*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K; v,body1,body2: untyped) =
  getPut do:
    var v {.inject.} = t.data[i].val.addr
    body1
  do:
    t.data[k].key = key
    var v {.inject.} = t.data[k].val.addr
    body2

template doAdd(body: untyped) {.dirty.} =
  if t.data.len == 0: t.init
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
  t.data[k].key = key
  body
  t.count.inc

proc add*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]; key: K) {.inline.} =
  doAdd: discard

proc add*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K, val: V) {.inline.} =
  doAdd: t.data[k].val = val

proc missingOrExcl*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K): bool =
  popRet do: t.rawDel i
  do: result = true

proc take*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]; key: var K): bool {.inline.} =
  popRet do: key = move(t.data[i].key); t.rawDel i; result = true
  do: discard

proc take*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K; val: var V): bool {.inline.} =
  popRet do:
    val = move(t.data[i].val)
    t.rawDel i
    result = true
  do: discard

proc pop*[K,Z;z:static[int]](t: var LPTabz[K,void,Z,z]): K {.inline.} =
  if t.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  for e in t:         # Cheaper than it may look due to early exit, BUT can get
    result = e        #..slow as a set empties out.  Could keep running "min ix"
    t.excl result     #..based on t.flag (cheaply updated by either ins/del.
    return            #..Also broken if dups are present. XXX

proc pop*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]): (K, V) {.inline.} =
  if t.data.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  for k,v in t:
    result[0] = k
    result[1] = v
    discard t.missingOrExcl(k)
    return            

proc clear*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]) =
  if t.data.len == 0: return
  t.count = 0
  when Z is void:
    zeroMem t.data[0].addr, t.data.len * t.data[0].sizeof
  else:
    if z == K(0):
      zeroMem t.data[0].addr, t.data.len * t.data[0].sizeof
    else:
      for i in 0 ..< t.data.len:
        t.data[i].key = default(K)
        t.data.unUse i
        when V isnot void:
          t.data[i].val = default(V)

iterator items*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): K =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield s.data[i].key

iterator mitems*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z]): var K =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i): yield s.data[i].key

iterator hcodes*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if isUsed(s.data, i):
      when Z is void:
        yield (i, s.data[i].hcode)
      else:
        yield (i, s.hash(i))

iterator allItems*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]; key: K): K =
  let L = s.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(s.hashHc(hc0), s.data.high):
    assert(s.len == L, "cannot change a set while iterating over it")
    if not isUsed(s.data, i): break
    if s.equal(i, key, hc0): yield s.data[i].key

iterator pairs*[K,Z;z:static[int]](t: LPTabz[K,void,Z,z]): tuple[a: int; key: K] =
  let L = t.len
  var j = 0
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield (j, t.data[i].key); j.inc

iterator pairs*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): (K,V) =
  let L = t.len
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield (t.data[i].key, t.data[i].val)

iterator mpairs*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]): var tuple[key: K, val: V] =
  let L = t.len
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield (t.data[i].key, t.data[i].val)

iterator keys*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): K =
  let L = t.len
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield t.data[i].key

iterator values*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]): V =
  let L = t.len
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield t.data[i].val

iterator mvalues*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z]): var V =
  let L = t.len
  for i in 0 ..< t.data.len:
    assert(t.len == L, "cannot change a tab while iterating over it")
    if isUsed(t.data, i): yield t.data[i].val

iterator allValues*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z]; key: K): V =
  let L = t.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc0), t.data.high):
    assert(t.len == L, "cannot change a tab while iterating over it")
    if not isUsed(t.data, i): break
    if t.equal(i, key, hc0): yield t.data[i].val

proc debugDump*[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]; label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ",
         if isUsed(s.data, i): depth(i, s.hash(i), s.data.high) else: 0,
         " ", cell

proc pop*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z]; key: var K): bool {.inline.} =
  s.take key

proc incl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.containsOrIncl(elt)

proc excl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.missingOrExcl(elt)

proc incl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], other: LPTabz[K,void,Z,z]) =
  for elt in other: s.incl elt

proc excl*[K,Z;z:static[int]](s: var LPTabz[K,void,Z,z], other: LPTabz[K,void,Z,z]) =
  for elt in other: s.excl elt

proc card*[K,Z;z:static[int]](s: LPTabz[K,void,Z,z]): int {.inline.} = s.len

proc union*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result = s1
  result.incl s2

proc intersection*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result.init min(s1.len, s2.len)
  for elt in s1:
    if elt in s2: result.incl elt

proc difference*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result.init
  for elt in s1:
    if elt notin s2: result.incl elt

proc symmetricDifference*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result = s1
  for item in s2:
    if result.containsOrIncl(item): result.excl item

proc `+`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] {.inline.} =
  s1.union s2
proc `*`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] {.inline.} =
  s1.intersection s2
proc `-`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] {.inline.} =
  s1.difference s2
proc `-+-`*[K,Z;z:static[int]](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] {.inline.} =
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

#XXX map has distinct output type, but ornate syntax to alter optional sentinel.
proc map*[K, A,Z;z:static[int]](data: LPTabz[K,void,Z,z], op: proc (x: K): A {.closure.}): LPTabz[A,void,Z,z] =
  result.init data.len
  for item in data: result.incl op(item)

proc toLPTabz*[K,Z;z:static[int]](keys: openArray[K], dups=false): LPTabz[K,void,Z,z] =
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

proc depthStats*[K,V,Z;z:static[int]](s: LPTabz[K,V,Z,z]): tuple[m1, m2: float; mx: int] = # non-central!
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

proc toLPTabz*[K,V: not void,Z;z:static[int]](pairs: openArray[(K,V)], dups=false): LPTabz[K,V,Z,z] =
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

proc pop*[K,V: not void,Z;z:static[int]](t: var LPTabz[K,V,Z,z]; key: K; val: var V):
    bool {.inline.} = t.take key, val

template withValue*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K; value, body: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.data[i].val.addr
    body

template withValue*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K; value, body1, body2: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.data[i].val.addr
    body1
  else: body2

proc `[]`*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.data[i].val
  else: raiseNotFound(key)

proc `[]`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K): var V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.data[i].val
  else: raiseNotFound(key)

proc `[]=`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K, val: V) =
  getPut do: t.data[i].val = val  # Replace FIRST FOUND item in multimap|insert
  do       : t.data[k].key = key; t.data[k].val = val

proc `{}`*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K): V {.inline.} = t[key]
proc `{}`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K): var V {.inline.} = t[key]
proc `{}=`*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K, val: V) {.inline.} = t[key] = val

proc hasKey*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K): bool {.inline.} = key in t

proc hasKeyOrPut*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K, val: V): bool {.inline.} =
  discard t.mgetOrPut(key, val, result)

proc getOrDefault*[K,V,Z;z:static[int]](t: LPTabz[K,V,Z,z], key: K, default=default(V)): V{.inline.}=
  mixin rawGet
  let i = t.rawGet(key)
  result = if i >= 0: t.data[i].val else: default

proc del*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K) {.inline.} =
  if t.missingOrExcl(key): key.raiseNotFound

proc del*[K,V,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K, had: var bool) {.inline.} =
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

proc indexBy*[A, K,V,Z;z:static[int]](collection: A, index: proc(x: V): K): LPTabz[K,V,Z,z] =
  result.init
  for item in collection: result[index(item)] = item

#A few things to maybe totally obviate CountTable or let it be a type alias
proc inc*[K,V: SomeInteger,Z;z:static[int]](t: var LPTabz[K,V,Z,z], key: K, amount: SomeInteger=1) {.inline.} =
  t.mgetOrPut(key, 0).inc amount

proc merge*[K,V: SomeInteger,Z;z:static[int]](s: var LPTabz[K,V,Z,z], t: LPTabz[K,V,Z,z]) =
  for key, val in t: s.inc(key, val)

iterator topPairsByVal*[K,V: SomeInteger,Z;z:static[int]](h: LPTabz[K,V,Z,z], n=10,
                                                          above=V.low): tuple[key: K; val: V] =
  var q = initHeapQueue[tuple[val: V; key: T]]()
  for key, val in h:
    if val > above:
      if q.len < n:
        q.push((val, key))
      elif (val, key) > q[0]:
        discard q.replace((val, key))
  var r: tuple[key: K; val: V]
  while q.len > 0:        # q now has top n entries
    let next = q.pop
    r.key = next.key
    r.val = next.val
    yield r               # yield in ascending order
