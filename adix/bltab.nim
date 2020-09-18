## This module is a specialization of `BLTab` to the Bit-Level case where keys
## are 1..k-bit ints & values are 0..v-bit ints (`k+v<=8*int.sizeof`) using one
## `SeqUInt` as backing store.  (Mnemonically, "BL" is also the start of "BLoom
## Filter", a sometimes competing data structure.)  Users must give a sentinel
## key.  Otherwise, `BLtab` is compatible with hash-ordered variants of BLTab
## multi(set|table)s.  (Multiset personality ensues when `v==0`, elsewise
## multitable personality.)

import althash, memutil, bitop, heapqueue, sequint, strutils, memfiles
export Hash, sequint
type
  BLTab* = object                   ## Optional Robin Hood Lin.Probe Hash Table
    data: SeqUInt                                 # RobinHoodOptional LP HashTab
    count: int                                    # count of used slots
    k, v, numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool                           # Steal 2-bits from `salt`?
    salt, z: Hash                                 # ~unpredictable salt,sentinel

var blInitialSize* = 2     ## default initial size aka capacity aka cap
var blNumer*       = 3     ## default numerator for lg(n) probe depth limit
var blDenom*       = 1     ## default denominator for lg(n) probe depth limit
var blMinFree*     = 1     ## default min free slots; (>= 1)
var blGrowPow2*    = 1     ## default growth power of 2; 1 means double
var blRehash*      = false ## default hcode rehashing behavior; auto-activated
var blRobinHood*   = false ## default to Robin Hood re-org; auto-activated

when defined(hashStats):   # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x  # all kind of like "times" - you v0=val;...; val-v0
  var blDepth*     = 0     ## Counts total search depth
  var blTooFull*   = 0     ## Counts resizes from minFree boundary
  var blTooDeep*   = 0     ## Counts resizes from deep probe sequences
  var blTooSparse* = 0     ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(blWarn) or not defined(danger):
  var blWarn*    = stderr  ## Set to wherever you want warnings to go
  var blMaxWarn* = 10      ## Most warnings per program invocation
  var blWarnCnt  = 0       # Running counter of warnings issued

proc save*(t: BLTab, pathStub: string) =
  # Save to a file named "`pathStub`.count-Rr[-salt]" for later load.
  var ext = "." & $t.count & "-" & (if t.robin: "r" else: "") &
                                   (if t.rehash: "R" else: "")
  if t.rehash: ext.add "-0x" & t.salt.toHex
  let f = system.open(pathStub & ext, fmWrite)
  let n = t.data.len
  let wr1 {.used.} = f.writeBuffer(n.unsafeAddr, n.sizeof)
  let wr2 {.used.} = f.writeBuffer(n.unsafeAddr, n.sizeof)
  let wr = f.writeBuffer(t.data[0].unsafeAddr, t.getCap * t.data[0].sizeof)
  if wr < t.getCap * t.data[0].sizeof:
    raise newException(IOError, "incomplete BLTab save")
  f.close

proc load*(t: var BLTab, path: string) =
  # Filename extension only encodes non-dynamic params. `setPolicy` adjusts.
  let ext   = path.split('.')[^1]
  let comps = ext.split('-')
  t.count  = parseInt(comps[0])
  t.robin  = 'R' in comps[1]
  t.rehash = 'r' in comps[1]
  if t.rehash and comps.len > 2: t.salt = parseInt(comps[2])
  let f  = system.open(path)
  let sz = f.getFileSize - 2*sizeof(int)
  t.data = newSeq[HCell(sz div sizeof(HCell[K,V,Z,z]))
  var junk: int
  discard f.readBuffer(junk.addr, sizeof(int))
  discard f.readBuffer(junk.addr, sizeof(int))
  let rd = f.readBuffer(t.data[0].addr, t.getCap * t.data[0].sizeof)
  f.close
  if rd < t.getCap * t.data[0].sizeof:
    raise newException(IOError, "truncated BLTab save file")
  t.pow2    = uint8(lg(t.getCap))
  t.numer   = uint8(blNumer)
  t.denom   = uint8(blDenom)
  t.minFree = uint8(blMinFree)

proc loadBLTab*(path: string): BLTab =
  result.load path

proc mmap*(t: var BLTab, path: string) =
  ##NOTE: Read-Only memory map; Attempted edits => run-time errors (like SEGV).
  let ext   = path.split('.')[^1]
  let comps = ext.split('-')
  t.count  = parseInt(comps[0])
  t.robin  = 'R' in comps[1]
  t.rehash = 'r' in comps[1]
  if t.rehash and comps.len > 2: t.salt = parseInt(comps[2])
  let mf {.used.} = memfiles.open(path)
  when defined(gcOrc):    #XXX Also need to block/hack destructor
    when defined(cpp):    #XXX This is all horribly GC-unsafe
      {.emit: """t.data.len = *(long *)`mf.mem`;
                 t.data.p = (void*)((char *)`mf.mem` + 2*`sizeof(int)`);""".}
    else:
      {.emit: """t->data.len = *(long *)`mf.mem`;
                 t->data.p = (void*)((char *)`mf.mem` + 2*`sizeof(int)`);""".}
  else:
    when defined(cpp):
      {.emit: "t.data = `mf.mem`;".}
    else:
      {.emit: "t->data = `mf.mem`;".}
  t.pow2    = uint8(lg(t.getCap))
  t.numer   = uint8(blNumer)
  t.denom   = uint8(blDenom)
  t.minFree = uint8(blMinFree)

proc len*(t: BLTab): int {.inline.} = t.count
proc high(t: BLTab): int {.inline.} = t.data.high
proc key(t: BLTab, i: int): K {.inline.} = t.data[i].key
proc getCap*(t: BLTab): int {.inline.} = t.data.len
proc isUsed(cell: HCell[K,V,Z,z]): bool {.inline.} = cell.key != z
proc isUsed(t: BLTab, i: int): bool {.inline.} = t.data[i].isUsed 

proc unUse(t: var BLTab, i: int, clear=true) {.inline.} = t.data[i].key = z

proc equal(t: BLTab; i: int, key: K, hc: Hash): bool {.inline.} =
  t.data[i].key == key

# These 4 do basic operations on the sparse table or compact index as relevant
proc pushUp(t: var BLTab, i, n: int) {.inline.} =
  for j in countdown(i + n - 1, i): t.data[j+1] = t.data[j]

proc set1(t: var BLTab; i, j: int) {.inline.} =
  t.data[i] = t.data[j]

proc pullDown(t: var BLTab, i, n: int) {.inline.} =
  for j in countup(i, i + n - 1): t.data[j] = t.data[j+1]

proc cell(t: BLTab, i: int): int {.inline.} = t.data[i]

proc hashHc(t: BLTab; hc: Hash): Hash {.inline.}=
  if t.rehash: hash(hc, t.salt) else: hc

proc hash(t: BLTab; i: int): Hash {.inline.} = t.hashHc hash(t.key(i))

proc hash0[K,Z](key: K): Hash {.inline.} =
  result = hash(key)
  if result == z: #XXX really result and mask == z
    result.inc

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

proc rawGet(t: BLTab; key: K; hc, d: var Hash): int {.inline.} =
  assert(t.getCap > 0, "Uninitialized BLTab")  # Ensure in *caller* not here
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
    ifStats blDepth.inc
  result = -1 - j               # < 0 => MISSING and insert idx = -1 - result

proc rawGetDeep(t: BLTab; key: K; hc, d: var Hash): int {.inline.} =
  assert(t.getCap > 0, "Uninitialized BLTab") # Ensure in *caller* not here
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
    ifStats blDepth.inc

proc rawGet(t: BLTab; key: K): int {.inline.} =
  var hc, d: Hash
  rawGet(t, key, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc rawGetLast(s: BLTab): int {.inline,used.} =
  let hc = hash0[K,Z](s.data[^1].key) # Can't get out of idx without a back-link
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    if int(s.idx[i] shr z) == s.data.len: return i

proc depth(t: BLTab; key: K): int {.inline.} =
  var hc, d: Hash
  discard rawGet(t, key, hc, d)
  d

proc depths*(t: BLTab): seq[int] =
  ## Compute & return exact distribution of search depths over a set
  for k in t.keys:
    let d = t.depth(k)
    if d >= result.len: result.setLen(d + 1)
    result[d] += 1

proc tooFull(t: var BLTab; d: int; newSize: var int): bool {.inline.} =
  result = true                 # Whether to call setCap or not
  if t.getCap - t.len < t.minFree.int + 1:
    dbg echo("Too little space (", t.getCap - t.len, ") of ", t.getCap)
    ifStats blTooFull.inc
    newSize = t.getCap shl t.growPow2
    return
  if t.denom.int * (d - 1) < t.numer.int * t.pow2.int:
    return false                # newSize will not matter
  dbg echo("Probe too deep: ",d," while lg(sz)=",t.pow2," depths: ",t.depths)
  ifStats blTooDeep.inc
  if t.len > t.getCap shr t.growPow2:
    newSize = t.getCap shl t.growPow2
  else:
    dbg echo("Too sparse to grow, ", t.len, "/", t.getCap, " depth: ", d)
    ifStats blTooSparse.inc     # Normal resizing cannot restore performance
    newSize = t.getCap
    var ext: string             # Extra text after primary message
    if t.robin:                 # Already doing Robin Hood re-org
      if t.rehash:              # Already re-hashing hash() output
        ext = "; Switch to tree|seq for many dups"
        result = false          # Could potentially auto-convert to B-tree here
      else:
        ext = "; Adapting by re-hashing hash()"
        t.rehash = true
    else:                       # Turn on re-hashing hash() output
      t.robin = true
      ext = "; Adapting by Robin Hood re-org"
    when defined(blWarn) or not defined(danger):
      blWarnCnt.inc
      if blWarnCnt <= blMaxWarn:
        blWarn.write "BLTab: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1(t: var BLTab; i: Hash; d: var int): int {.inline.} =
  if t.robin:
    result = i                          # Linear probe to first empty slot
    while t.isUsed(result):
      result = (result + 1) and t.high
      d.inc

proc rawPut2(t: var BLTab; i, j: Hash): int {.inline.} =
  if t.robin:
    if j > i:                           # No table wrap around; just shift up
      t.pushUp i, j - i
    elif j < i:                         # j wrapped to low indices
      t.pushUp 0, j
      t.set1 0, t.high
      t.pushUp i, t.high - i
  # else:                               # j == i => already have space @i; done
  result = i

proc ixHc(i: int; hc: Hash; z: int): int {.inline.} =
  int(i shl z) or int(hc and ((Hash(1) shl z) - 1))

proc rawRawDel(t: var BLTab; i: Hash) {.inline.}=
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
    block outer:
      while true:         # The correctness of this depends on (i+1) in probeSeq
        var j = i         #..though may be adaptable to other simple sequences.
        t.unUse i                       # Mark current FREE
        while true:
          i = (i + 1) and mask          # Increment mod table size
          if not t.isUsed(i):           # End of collision cluster; All done
            break outer
          let h = t.hash(i) and mask    # "home" slot of key@i
          if not((i >= h and h > j) or (h > j and j > i) or (j > i and i >= h)):
            break
        t.set1 j, i                     # data[i] will be marked FREE next loop

proc rawDel(t: var BLTab; i: Hash) {.inline.} =
  when not (Z is K or Z is void):
    let j = (t.idx[i] shr z) - 1        # To keep `data` density..
    let jp1 = j.int + 1
    if jp1 < t.len:                     # ..unless already points to data[^1]
      let m  = t.rawGetLast
      let hc = cast[Hash](t.idx[m])
      t.idx[m] = ixHc(jp1, hc, z)       # ..retarget idx[data[^1]] -> j
      t.data[j] = t.data[^1]            # Copy last elt to `[j]`; move?
  t.rawRawDel(i)
  when not (Z is K or Z is void):
    discard t.data.pop

template getPut(t, i, k, key, present, missing: untyped) =
  mixin rawPut1, rawPut2, tooFull
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
      t.idx[k] = ixHc(t.data.len + 1, hc, z)
      t.data.setLen t.data.len + 1
    missing
  else:
    present

template popRet(t, i, key, present, missing: untyped) =
  var i: int
  if t.getCap == 0 or (i = t.rawGet(key); i) < 0:
    missing
  else:
    when compiles(t.count):
      t.count.dec
    present

proc slotsGuess(count: int, minFree=blMinFree): int {.inline.} =
  ceilPow2(count + minFree) # Might have a great hash

proc init*(t:var BLTab; initialSize=blInitialSize, numer=blNumer, denom=blDenom,
           minFree=blMinFree, growPow2=blGrowPow2, rehash=blRehash,
           robinhood=blRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize, minFree)
  t.data     = newSeq[HCell(initialSize)
  if z != 0:
    for i in 0 ..< initialSize: t.unUse(i, false)
  t.salt     = getSalt(t.data[0].addr)
  t.count    = 0
  t.numer    = numer.uint8
  t.denom    = denom.uint8
  t.minFree  = max(1, minFree).uint8  #Block user allowing inf loop possibility
  t.growPow2 = uint8(growPow2)
  t.pow2     = uint8(initialSize.lg)
  t.rehash   = rehash
  t.robin    = robinhood

proc initBLTab*(initialSize=blInitialSize, numer=blNumer, denom=blDenom,
                minFree=blMinFree, growPow2=blGrowPow2, rehash=blRehash,
                robinhood=blRobinHood): BLTab {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*(t: var BLTab; numer=blNumer, denom=blDenom, minFree=blMinFree,
                growPow2=blGrowPow2, rehash=blRehash,
                robinhood=blRobinHood) {.inline.} =
  ## Must call ``setCap`` after changing certain params here (e.g. ``rehash``).
  t.numer    = numer
  t.denom    = denom
  t.minFree  = minFree
  t.growPow2 = growPow2
  t.rehash   = rehash
  t.robin    = robinhood

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc setCap*(t: var BLTab; newSize = -1) =
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
    var old = newSeq[HCell(newSz)
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
    t.idx = initSeqUint(newSz, newSz shl z)
    if t.rehash: t.salt = getSalt(t.idx.addr0)
    var hc: Hash            #XXX Must loop over idx[]!=0 for hc's OR re-calc if
    for i, cell in t.data:  #table > z bits BUT also want data/ins.ord.  Save
      var d: Hash = 0       #full hcode iff Z is OrdCostlyKey is best solution.
      let j = t.rawGetDeep(cell.key, hc, d)
      t.idx[t.rawPut2(j, t.rawPut1(j, d))] = ixHc(i + 1, hc, z)
    dbg echo(" NEW SALT: ", t.salt)

proc contains*(t: BLTab; key: K): bool {.inline.} =
  t.getCap > 0 and t.rawGet(key) >= 0

proc containsOrIncl*(t: var BLTab; key: K): bool {.inline.} =
  t.getPut(i, k, key) do: result = true
  do: result = false; t.cell(k)[].key = key

proc setOrIncl*(t: var BLTab; key: K): bool {.inline.} =
  t.getPut(i, k, key) do: t.cell(i)[].key = key; result = true
  do: t.cell(k)[].key = key; result = false

proc mgetOrPut*(t: var BLTab; key: K; val: V): var V {.inline.} =
  t.getPut(i, k, key) do: result = t.cell(i)[].val
  do: (let c = t.cell(k); c[].key = key; c[].val = val; result = c[].val)

proc mgetOrPut*(t: var BLTab; key: K; val: V; had: var bool): var V {.inline.} =
  t.getPut(i, k, key) do:
    had=true ; result = t.cell(i)[].val
  do:
    had=false; (let c = t.cell(k)); c[].key = key; c[].val = val
    result = c[].val

template editOrInit*(t: var BLTab; key: K; v, body1, body2: untyped) =
  mixin cell, getPut
  getPut(t, i, k, key) do:
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
    t.idx[i] = ixHc(t.data.len + 1, hc, z)
    t.data.setLen t.data.len + 1
  t.cell(k)[].key = key
  body

proc add*(t: var BLTab; key: K) {.inline.} =
  doAdd: discard

proc add*(t: var BLTab; key: K, val: V) {.inline.} =
  doAdd: t.cell(k)[].val = val

proc missingOrExcl*(t: var BLTab; key: K): bool =
  t.popRet(i, key) do: t.rawDel i
  do: result = true

proc take*(t: var BLTab; key: var K): bool {.inline.} =
  t.popRet(i, key) do: key = move(t.cell(i)[].key); t.rawDel i; result = true
  do: discard

proc take*(t: var BLTab; key: K; val: var V): bool {.inline.} =
  t.popRet(i, key) do:
    val = move(t.cell(i)[].val)
    t.rawDel i
    result = true
  do: discard

proc pop*(t: var BLTab): K {.inline.} =
  if t.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  when Z is K or Z is void:
    for i in 0 ..< t.getCap:
      if t.isUsed(i):             # Slows down as a set empties (as does regular
        result = t.data[i].key    #..iteration).  Could keep a running "min ix"
        t.rawDel i                #..updated by ins/del ops. XXX Performance
        return                    # Early exit => cheaper than it may look
  else:
    result = t.data[^1].key       # seq pop avoids O(len) shift on data
    t.rawDel t.rawGetLast         # does the s.data.pop internally

proc pop*(t: var BLTab): (K, V) {.inline.} =
  if t.len == 0: raise newException(IndexError, formatErrorIndexBound(0,0))
  when Z is K or Z is void:
    for i in 0 ..< t.getCap:      # See performance note for unkeyed set pop
      if t.isUsed(i):
        result[0] = t.data[i].key
        result[1] = t.data[i].val
        t.rawDel i
        return
  else:                           # This branch should delete the right dup.
    result[0] = t.data[^1].key    # seq pop avoids O(len) shift on data
    result[1] = t.data[^1].val
    t.rawDel t.rawGetLast         # does the s.data.pop internally

proc editKey*(t: var BLTab; old, new: K) {.inline.} =
  ##Insertion-ordered tables support editing keys while keeping iteration order.
  when Z is K or Z is void:
    assert false, "hash-order tables do not support key edits"
  else:
    let i = t.rawGet(old)
    if i < 0: raiseNotFound(old)
    let k = int((t.idx[i] shr z) - 1)
    t.rawRawDel i
    t.data[k].key = new
    var hc, d: Hash
    let j = t.rawGetDeep(new, hc, d)  #Allow `new` to be a dup key
    var newSize: int
    if t.tooFull(d, newSize):
      t.setCap newSize                #`new` already in `data`; setCap does rest
    else:                             #else point-edit new key index into table
      t.idx[t.rawPut2(j, t.rawPut1(j, d))] = ixHc(k + 1, hc, z)

proc nthKey*(t: BLTab; n: int): K {.inline.} =
  ## Insertion-ordered multisets support 0-origin nth-in-order key.
  when Z is K or Z is void:
    assert false, "hash-order multisets do not support ins-order query"
  else:
    t.data[n].key

proc nthPair*(t: BLTab; n: int): (K, V) {.inline.} =
  ## Insertion-ordered tables support 0-origin nth-in-order pair.
  when Z is K or Z is void:
    assert false, "hash-order multitables do not support ins-order query"
  else:
    (t.data[n].key, t.data[n].val)

proc nthPair*(t: var BLTab; n: int): (K, ptr V) {.inline.} =
  ## Insertion-ordered tables support 0-origin nth-in-order pair w/editable val.
  when Z is K or Z is void:
    assert false, "hash-order multitables do not support ins-order query"
  else:
    (t.data[n].key, t.data[n].val.addr)

proc clear*(t: var BLTab) =
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

iterator cells(s: BLTab): HCell[K,V,Z,z] =
  let L = s.len
  if s.len > 0:
    for cell in s.data:
      assert(s.len == L, "cannot edit while iterating")
      when Z is K or Z is void:
        if not cell.isUsed: continue
      yield cell

iterator mcells(s: var BLTab):ptr HCell[K,V,Z,z]=
  let L = s.len
  if s.len > 0:
    for i in 0 ..< s.data.len:
      assert(s.len == L, "cannot edit while iterating")
      when Z is K or Z is void:
        if not s.isUsed(i): continue
      yield s.data[i].addr

iterator items*(s: BLTab): K =
  for cell in s.cells: yield cell.key

iterator mitems*(s: var BLTab): var K =
  for cell in s.mcells: yield cell.key

iterator hcodes*(s: BLTab): (int, Hash) =
  when Z is void:
    for cell in s.cells: yield (i, cell.hcode)
  else:
    for i, cell in 0 ..< s.getCap:
      if s.isUsed(i): yield (i, s.hash(i))

iterator allItems*(s: BLTab; key: K): K =
  let L = s.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(s.hashHc(hc0), s.high):
    assert(s.len == L, "cannot edit while iterating")
    if not s.isUsed(i): break
    if s.equal(i, key, hc0): yield s.key(i)

proc numItems*(t: BLTab; key: K): int{.inline.}=
  for key in t.allItems(key): result.inc

iterator numItems*(t: BLTab): (K, int) =
  for key in t.keys: yield (key, t.numItems(key))

iterator pairs*(t: BLTab): (int, K) =
  var j = 0
  for cell in t.cells: yield (j, cell.key); j.inc

iterator pairs*(t: BLTab): (K,V) =
  for cell in t.cells: yield (cell.key, cell.val)

iterator mpairs*(t: var BLTab): (K, var V) =
  for cell in t.mcells: yield (cell.key, cell.val)

iterator keys*(t: BLTab): K =
  for cell in t.cells: yield cell.key

iterator values*(t: BLTab): V =
  for cell in t.cells: yield cell.val

iterator mvalues*(t: var BLTab): var V =
  for cell in t.mcells: yield cell.val

iterator allValues*(t: BLTab; key: K): V =
  let L = t.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc0), t.high):
    assert(t.len == L, "cannot change a tab while iterating over it")
    if not t.isUsed(i): break
    if t.equal(i, key, hc0): yield t.cell(i)[].val

iterator allValues*(t: BLTab; vals: var seq[V]): K =
  for key in t.keys:
    vals.setLen 0
    for val in t.allValues(key): vals.add val
    yield key

proc allValues*(t: BLTab; key: K, vals: var seq[V]): bool {.inline.} =
  vals.setLen 0
  for val in t.allValues(key): vals.add val
  vals.len > 0

proc debugDump*(s: BLTab; label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ",
         if s.isUsed(i): depth(i, s.hash(i), s.high) else: 0, " ", cell
  when compiles(s.idx):
    for i, j in s.idx:
      echo "  i: ", i, " depth: ",
           (if j == 0: 0 else: depth(i, s.hash(i), s.idx.high)),
           " idx: ", j shr z, " hc: ", j and ((1 shl z) - 1)

proc pop*(s: var BLTab; key: var K): bool {.inline.} = s.take key

proc incl*(s: var BLTab, elt: K) {.inline.} =
  discard s.containsOrIncl(elt)

proc excl*(s: var BLTab, elt: K) {.inline.} =
  discard s.missingOrExcl(elt)

proc incl*(s: var BLTab, other: BLTab) =
  for elt in other: s.incl elt

proc excl*(s: var BLTab, other: BLTab) =
  for elt in other: s.excl elt

proc card*(s: BLTab): int {.inline.} = s.len

proc union*(s1, s2: BLTab): BLTab =
  result = s1
  result.incl s2

proc intersection*(s1, s2: BLTab): BLTab =
  result.init min(s1.len, s2.len)
  for elt in s1:
    if elt in s2: result.incl elt

proc difference*(s1, s2: BLTab): BLTab =
  result.init
  for elt in s1:
    if elt notin s2: result.incl elt

proc symmetricDifference*(s1, s2: BLTab): BLTab =
  result = s1
  for item in s2:
    if result.containsOrIncl(item): result.excl item

proc `+`*(s1, s2: BLTab): BLTab {.inline.} =
  s1.union s2
proc `*`*(s1, s2: BLTab): BLTab {.inline.} =
  s1.intersection s2
proc `-`*(s1, s2: BLTab): BLTab {.inline.} =
  s1.difference s2
proc `-+-`*(s1, s2: BLTab): BLTab {.inline.} =
  s1.symmetricDifference s2

proc disjoint*(s1, s2: BLTab): bool =
  for item in s1:
    if item in s2: return
  result = true

proc `<=`*(s, t: BLTab): bool =
  if s.len == 0: return t.len == 0
  if t.len == 0: return false             # s non-nil, but t nil
  var keyLast: K                          # multiset .len uninformative as to s
  var first = true                        #..having < #keys but ># degeneracies.
  for key, snum in s.numItems:            # Generalization of subsetness w/dups
    if first or key != keyLast:           #..means count in s <= count in t
      first = false                       #..and every key in s is also in t.
      if t.numItems(key) < snum: return false
      keyLast = key
  result = true

proc `<`*(s, t: BLTab): bool =
  s.len != t.len and s <= t

proc `==`*(s, t: BLTab): bool =
  if s.len == 0: return t.len == 0        # 2 nil => true
  if t.len == 0: return false             # 1 nil => false
  if s.len != t.len: return false         # diff size => false
  var keyLast: K
  var first = true
  for key, snum in s.numItems:
    if first or key != keyLast:
      first = false
      if t.numItems(key) != snum: return false
      keyLast = key
  return true

proc map*(data: BLTab, op: proc (x: K): A {.closure.}): BLTab =
  result.init data.len
  for item in data: result.incl op(item)

proc toBLTab*(keys: openArray[K], dups=false): BLTab =
  result.init keys.len
  if dups:
    for item in keys: result.add item   # append; multi-set
  else:
    for item in keys: result.incl item  # replace; unique set

proc `$`*(s: BLTab): string =
  if s.len == 0: return "{}"
  result = "{"
  for key in s:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
  result.add("}")

proc hash*(s: BLTab): Hash =
  for i, hc in 0 .. s.hcodes: result = result xor hc
  result = !$result  #Important to use a COMMUTATIVE combiner above

proc depthStats*(s: BLTab): tuple[m1, m2: float; mx: int] = # non-central!
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

proc toBLTab*(pairs: openArray[(K,V)], dups=false): BLTab =
  result.init pairs.len
  if dups:
    for key, val in items(pairs): result.add(key, val) # append; multi-table
  else:
    for key, val in items(pairs): result[key] = val    # replace

proc `$`*(t: BLTab): string =
  if t.len == 0: return "{:}"
  result = "{"
  for key, val in t:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
    result.add(": ")
    result.addQuoted(val)
  result.add("}")

proc pop*(t: var BLTab; key: K; val: var V): bool {.inline.} =
  t.take key, val

template withValue*(t: var BLTab, key: K; value, body: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    body

template withValue*(t: var BLTab, key: K; value, body1, body2: untyped) =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    body1
  else: body2

proc `[]`*(t: BLTab, key: K): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: raiseNotFound(key)

proc `[]`*(t: var BLTab, key: K): var V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: result = t.cell(0).val; raiseNotFound(key)

proc `[]=`*(t: var BLTab, key: K, val: V) {.inline.} =
  t.getPut(i,k,key) do: t.cell(i)[].val = val #Replace 1st FOUND elt in multimap|put
  do: (let c = t.cell(k); c[].key = key; c[].val = val)

proc `{}`*(t: BLTab, key: K): V {.inline.} = t[key]
proc `{}`*(t: var BLTab, key: K): var V {.inline.} = t[key]
proc `{}=`*(t: var BLTab, key: K; val: V) {.inline.} = t[key] = val

proc hasKey*(t: BLTab, key: K): bool {.inline.} = key in t

proc hasKeyOrPut*(t: var BLTab, key: K, val: V): bool {.inline.} =
  discard t.mgetOrPut(key, val, result)

proc getOrDefault*(t: BLTab, key: K, default=default(V)): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  result = if i >= 0: t.cell(i)[].val else: default

proc del*(t: var BLTab, key: K) {.inline.} =
  if t.missingOrExcl(key): key.raiseNotFound

proc del*(t: var BLTab, key: K, had: var bool) {.inline.} =
  had = not t.missingOrExcl(key)

proc `==`*(x, y: BLTab): bool =
  if x.len == 0: return y.len == 0        # 2 nil => true
  if y.len == 0: return false             # 1 nil => false
  if x.len != y.len: return false         # diff size => false
  var xvals, yvals: seq[V]
  var keyLast: K
  var first = true
  for key in x.allValues(xvals):
    if first or key != keyLast:
      first = false
      if not y.allValues(key, yvals) or yvals != xvals:
        return false
      keyLast = key
  return true

proc indexBy*[A](collection: A, index: proc(x: int): int): BLTab =
  result.init
  for item in collection: result[index(item)] = item

#A few things to maybe totally obviate CountTable or let it be a type alias
proc inc*(t: var BLTab, key: K, amount: SomeInteger=1) {.inline.} =
  t.editOrInit(key, val):
    val[] += amount
    if val[] == 0: c.del key
  do:
    val[] = amount

proc merge*(c: var BLTab, b: BLTab) =
  for key, val in b: c.inc(key, val)

iterator topByVal*(c: BLTab, n=10, min=V.low): (K, V) =
  var q = initHeapQueue[(V, K)]()
  for key, val in c:
    if val >= min:
      let e = (val, key)
      if q.len < n: q.push(e)
      elif e > q[0]: discard q.replace(e)
  var y: (K, V)
  while q.len > 0:        # q now has top n entries
    let r = q.pop
    y[0] = r[1]
    y[1] = r[0]
    yield y               # yield in ascending order
