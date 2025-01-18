## This module provides an (in|un)ordered multiset/multitable representation via
## Linear Probing with aging-friendly Backshift Delete(Knuth TAOCPv3) & optional
## Robin Hood re-org (Celis,1986).  Linear probing collision clusters yields "no
## tuning needed" locality of reference: 1 DRAM hit per access for large tables
## of small items.  RH sorts collision clusters by search depth which adds nice
## properties: faster miss search (eg. for inserts, usually compensating for
## data motion) and min depth variance (no unlucky keys).  The latter enables
## ~100% table space utilization (we need one empty slot to halt some loops).
##
## Misuse/attack is always possible.  Inserting *many* duplicate causes overlong
## scans just as hash collisions do and is thus misuse/self-attack.  If this is
## likely then use `V=seq[T]` instead.  We provide a few mitigations triggered,
## like table growth itself, by overlong scans on underfull tables:
##   A) automatic rehash of user `hash` output with a strong integer hash,
##   B) overlong scan warnings (disabled in `danger` mode),
##   C) automatic Robin Hood re-org activation, and
##   D) use `althash.getSalt` to allow hard to predict per-table hash salt.
## Program-wide-tunable defaults are to rehash, warn, re-org & salt with vmAddr
## since this is a safe-ish portable mode, but most can also be set via module
## globals consulted @init time.  `-d:unstableHash` makes the default `getSalt`
## a secure random number via `std/sysrand`, but debugging may be harder.
##
## MultiSET personality ensues when the `V` value type generic parameter is
## `void`.  Otherwise the style of interface is multiTABLE.  Every attempt is
## made for either personality to be drop-in compatible with Nim's standard
## library sets & tables, but extra features are provided here.
##
## Space-time optimization of a sentinel key (a value of `K` disallowed for
## ordinary keys) is supported through the final two generic parameters, `Z`,
## and `z`.  If `Z` is `void`, hash codes are saved and `z` is ignored.  If
## `Z==K`, `z` is the sentinel key value.
##
## If `Z` is neither `K` nor `void` then compact, insertion-ordered mode is used
## and `z` means how many bits of hcode are saved beside an index into a dense
## `seq[(K,V)]`.  6..8 bits avoids most "double cache misses" for miss
## lookups/inserts. `z=0` works if space matters more than time.

import althash, memutil, bitop, topk, sequint, std/[strutils, memfiles]
export Hash, sequint, topk.TopKOrder
when not declared(assert): import std/[assertions, objectdollar]
when declared(File):
  template stdOpen(x: varargs[untyped]): untyped = system.open(x)
else:
  import std/syncio; export syncio
  template stdOpen(x: varargs[untyped]): untyped = syncio.open(x)
type                  ## `K` is Key type; `V` is Value type (can be `void`)
  HCell[K,V,Z;z:static int] = object
    when Z is void:   ## void sentinel type => no sentinel; else `z` is sentinel
      hcode: int      #NOTE: ins-order mode hcodes, if any, are in idx[]
    key: K
    when V isnot void:
      val: V

  LPTabz*[K,V,Z;z:static int] = object            ## Robin Hood Hash Set
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
  LPSetz*[K,Z;z:static int] = LPTabz[K,void,Z,z]  ## LPTabz for sentinel Set
  LPTab*[K,V] = LPTabz[K,V,void,0]                ## LPTabz for no-sentinel Tab
  LPSet*[K] = LPTabz[K,void,void,0]               ## LPTabz for no-sentinel Set

var lpInitialSize* = 2 ## default initial size aka capacity aka cap
var lpNumer*       = 3 ## default numerator for lg(n) probe depth limit
var lpDenom*       = 1 ## default denominator for lg(n) probe depth limit
var lpMinFree*     = 1 ## default min free slots; (>= 1)
var lpGrowPow2*    = 1 ## default growth power of 2; 1 means double
var lpRobinHood*   = false ## default to Robin Hood re-org; auto-activated
var lpRehash*      = false ## default hcode rehashing behavior; auto-activated

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

proc save*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z], pathStub: string) =
  # Save to a file named "`pathStub`.count-Rr[-salt]" for later load.
  when Z is K or Z is void:
    var ext = "." & $t.count & "-" & (if t.robin: "r" else: "") &
                                     (if t.rehash: "R" else: "")
    if t.rehash:
      ext.add "-0x" & t.salt.toHex
    let f = stdOpen(pathStub & ext, fmWrite)
    let n = t.data.len
    let wr1 {.used.} = f.writeBuffer(n.unsafeAddr, n.sizeof)
    let wr2 {.used.} = f.writeBuffer(n.unsafeAddr, n.sizeof)
    let wr = f.writeBuffer(t.data[0].unsafeAddr, t.getCap * t.data[0].sizeof)
    if wr < t.getCap * t.data[0].sizeof:
      raise newException(IOError, "incomplete LPTabz save")
    f.close
  else:
    assert "only implemented for one-level LPTabz"

proc load*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], path: string) =
  # Filename extension only encodes non-dynamic params. `setPolicy` adjusts.
  when Z is K or Z is void:
    let ext   = path.split('.')[^1]
    let comps = ext.split('-')
    t.count  = parseInt(comps[0])
    t.robin  = 'R' in comps[1]
    t.rehash = 'r' in comps[1]
    if t.rehash and comps.len > 2: t.salt = parseInt(comps[2])
    let f  = stdOpen(path)
    let sz = f.getFileSize - 2*sizeof(int)
    t.data = newSeq[HCell[K,V,Z,z]](sz div sizeof(HCell[K,V,Z,z]))
    var junk: int
    discard f.readBuffer(junk.addr, sizeof(int))
    discard f.readBuffer(junk.addr, sizeof(int))
    let rd = f.readBuffer(t.data[0].addr, t.getCap * t.data[0].sizeof)
    f.close
    if rd < t.getCap * t.data[0].sizeof:
      raise newException(IOError, "truncated LPTabz save file")
    t.pow2    = uint8(lg(t.getCap))
    t.numer   = uint8(lpNumer)
    t.denom   = uint8(lpDenom)
    t.minFree = max(1, lpMinFree).uint8
  else:
    assert "only implemented for one-level LPTabz"

proc loadLPTabz*[K,V,Z;z:static int](path: string): LPTabz[K,V,Z,z] =
  result.load path

proc mmap*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], path: string) =
  ##NOTE: Read-Only memory map; Attempted edits => run-time errors (like SEGV).
  when Z is K or Z is void:
    let ext   = path.split('.')[^1]
    let comps = ext.split('-')
    t.count  = parseInt(comps[0])
    t.robin  = 'R' in comps[1]
    t.rehash = 'r' in comps[1]
    if t.rehash and comps.len > 2: t.salt = parseInt(comps[2])
    let mf {.used.} = memfiles.open(path)
    when defined(gcOrc)or defined(gcArc): #XXX Should also block/hack destructor
      when defined(cpp):                  #XXX This is all horribly GC-unsafe
        {.emit: """`t`.data.len = *(long *)`mf.mem`;
                   `t`.data.p = (void*)((char *)`mf.mem`+2*`sizeof(int)`);""".}
      else:
        {.emit: """`t`->data.len = *(long *)`mf.mem`;
                   `t`->data.p = (void*)((char *)`mf.mem`+2*`sizeof(int)`);""".}
    else:
      when defined(cpp):
        {.emit: "`t`.data = `mf.mem`;".}
      else:
        {.emit: "`t`->data = `mf.mem`;".}
    t.pow2    = uint8(lg(t.getCap))
    t.numer   = uint8(lpNumer)
    t.denom   = uint8(lpDenom)
    t.minFree = max(1, lpMinFree).uint8
  else:
    assert "only implemented for one-level LPTabz"

proc len*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.count
  else:
    t.data.len

proc high[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.data.high
  else:
    t.idx.high

proc key[K,V,Z;z:static int](t: LPTabz[K,V,Z,z], i: int): K {.inline.} =
  when Z is K or Z is void:
    t.data[i].key
  else:
    t.data[(t.idx[i] shr z) - 1].key

proc getCap*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): int {.inline.} =
  when Z is K or Z is void:
    t.data.len
  else:
    t.idx.len

proc isUsed[K,V,Z;z:static int](cell: HCell[K,V,Z,z]): bool {.inline.} =
  when Z is void:
    cell.hcode != 0
  else:
    cell.key != K(z)

proc isUsed[K,V,Z;z:static int](t: LPTabz[K,V,Z,z], i: int): bool {.inline.} =
  when Z is K or Z is void:
    t.data[i].isUsed
  else:
    t.idx[i] != 0

proc unUse[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], i: int,
                               clear=true) {.inline.} =
  when Z is K or Z is void:
    when Z is void:
      t.data[i].hcode = 0
      when compiles(default(t.data[i].val)):
        if clear:
          t.data[i].key = default(t.data[i].key)
    else:
      t.data[i].key = K(z)
    when V isnot void and compiles(default(t.data[i].val)):
      if clear:
        t.data[i].val = default(t.data[i].val)
  else:
    t.idx[i] = 0

proc equal[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; i: int, key: K, hc: Hash):
    bool {.inline.} =
  when Z is K or Z is void:
    when type(key) is SomeInteger or Z isnot void:
      t.data[i].key == key
    else: # Compare hc 1st so missing => ~0 key cmp; Present => ~1 key cmp
      t.data[i].hcode == hc and t.data[i].key == key
  else:
    let msk = (1 shl z) - 1
    let hq = hc and msk
    let hk = int(t.idx[i]) and msk
    hq == hk and t.data[(t.idx[i] shr z) - 1].key == key

# These 4 do basic operations on the sparse table or compact index as relevant
proc pushUp[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], i, n: int) {.inline.} =
  when Z is K or Z is void:
    t.data.pushUp i, n
  else:
    for j in countdown(i + n - 1, i): t.idx[j+1] = t.idx[j]

proc set1[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; i, j: int) {.inline.} =
  when Z is K or Z is void:
    t.data[i] = t.data[j]
  else:
    t.idx[i] = t.idx[j]

proc pullDown[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], i, n: int){.inline.}=
  when Z is K or Z is void:
    t.data.pullDown i, n
  else:
    for j in countup(i, i + n - 1): t.idx[j] = t.idx[j+1]

proc cell[K,V,Z;z:static int](t: LPTabz[K,V,Z,z],
                              i: int): ptr HCell[K,V,Z,z] {.inline.} =
  when Z is K or Z is void:             # Get ptr to whole cell from slot num
    t.data[i].unsafeAddr
  else:
    t.data[(t.idx[i] shr z) - 1].unsafeAddr

proc hashHc[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; hc: Hash): Hash {.inline.}=
  if t.rehash: hash(hc, t.salt) else: hc

proc hash[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; i: int): Hash {.inline.} =
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
  cast[Hash]((i - hc) and mask)           # Search depth of entry w/hcode @ix`i`

iterator probeSeq(hc, mask: Hash): int =  # WTF generic over K caused codegenbug
  var i: Hash = hc and mask               # Start w/home address
  while true:
    yield i
    i = (i + 1) and mask                  # Linear Probing

proc rawGet[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K;
                                hc, d: var Hash): int {.inline.} =
  assert(t.getCap > 0, "Uninitialized LPTabz")  # Ensure in *caller* not here
  hc = hash0[K,Z](key)
  var j {.noinit.}: int                         # Where to insert if missing
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

proc rawGetDeep[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K;
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

proc rawGet[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K): int {.inline.} =
  var hc, d: Hash
  rawGet(t, key, hc, d)        # < 0 => MISSING and insert idx = -1 - result

proc rawGetLast[K,V,Z;z:static int](s: LPTabz[K,V,Z,z]): int {.inline,used.} =
  let hc = hash0[K,Z](s.data[^1].key) # Can't get out of idx without a back-link
  for i in probeSeq(s.hashHc(hc), s.idx.high):
    if int(s.idx[i] shr z) == s.data.len: return i

proc depth[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K): int {.inline.} =
  var hc, d: Hash
  discard rawGet(t, key, hc, d)
  d

proc depths*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): seq[int] =
  ## Compute & return exact distribution of search depths over a set
  for k in t.keys:
    let d = t.depth(k)
    if d >= result.len: result.setLen(d + 1)
    result[d] += 1

proc tooFull[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; d: int;
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
    when defined(lpWarn) or not defined(danger):
      lpWarnCnt.inc
      if lpWarnCnt <= lpMaxWarn:
        lpWarn.write "LPTabz: Weak hash/too many dups(d=" & $d & ")", ext, '\n'

proc rawPut1[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; i: Hash; d: var int):
    int {.inline.} =
  if t.robin:
    result = i                          # Linear probe to first empty slot
    while t.isUsed(result):
      result = (result + 1) and t.high
      d.inc

proc rawPut2[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; i, j: Hash): int =
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

proc rawRawDel[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; i: Hash) {.inline.}=
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

proc rawDel[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; i: Hash) {.inline.} =
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

proc slotsGuess(count: int, minFree: int): int {.inline.} =
  ceilPow2(count + max(1, minFree)) # Might have a great hash

proc init*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z];
    initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
    growPow2=lpGrowPow2, rehash=lpRehash, robinhood=lpRobinHood) {.inline.} =
  let initialSize = slotsGuess(initialSize, minFree)
  t.data     = newSeq[HCell[K,V,Z,z]](initialSize)
  when Z is K:
    if K(z) != K(0):
      for i in 0 ..< initialSize: t.unUse(i, false)
  when Z is K or Z is void:
    t.salt   = getSalt(t.data[0].addr)
    t.count  = 0
  else:
    t.idx    = initSeqUint(initialSize, initialSize shl z)
    t.salt   = getSalt(t.idx.addr0)
    t.data.setLen 0                   #Should make above newSeq like newSeqOfCap
  t.numer    = numer.uint8
  t.denom    = denom.uint8
  t.minFree  = max(1, minFree).uint8  #Block user allowing inf loop possibility
  t.growPow2 = uint8(growPow2)
  t.pow2     = uint8(initialSize.lg)
  t.rehash   = rehash
  t.robin    = robinhood

proc setCap*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; newSize = -1) =
  if t.getCap == 0: t.init
  var newSz: int
  if newSize < 0:
    newSz = t.getCap shl t.growPow2
    t.pow2 += t.growPow2
  else: # `max()` below blocks shrinking capacity below what is contained.
    newSz = slotsGuess(max(newSize, t.len), t.minFree.int)
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
      if K(z) != K(0):
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

template getPut(t, i, key, present, missing: untyped): untyped =
  mixin rawPut1, rawPut2, tooFull, getCap, init, setCap
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
    let i = k # Create an alias so missing & present bodies can both use `i`
    missing
  else:
    present

template popRet(t, i, key, present, missing: untyped): untyped =
  mixin getCap
  var i: int
  if t.getCap == 0 or (i = t.rawGet(key); i) < 0:
    missing
  else:
    when compiles(t.count):
      t.count.dec
    present

proc initLPTabz*[K,V,Z;z:static int](initialSize=lpInitialSize, numer=lpNumer,
    denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
    robinhood=lpRobinHood): LPTabz[K,V,Z,z] {.inline.} =
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

proc setPolicy*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; numer=lpNumer,
    denom=lpDenom, minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
    robinhood=lpRobinHood) {.inline.} =
  ## Must call `setCap` after changing certain params here (e.g. `rehash`).
  t.numer    = uint8(numer)
  t.denom    = uint8(denom)
  t.minFree  = max(1, minFree).uint8
  t.growPow2 = uint8(growPow2)
  t.rehash   = rehash
  t.robin    = robinhood

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc contains*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K):
    bool {.inline.} =
  t.getCap > 0 and t.rawGet(key) >= 0

proc containsOrIncl*[K,Z;z:static int](t: var LPTabz[K,void,Z,z];
                                       key: K): bool {.inline.} =
  t.getPut(i, key) do: result = true
  do: result = false; t.cell(i).key = key

proc setOrIncl*[K,Z;z:static int](t: var LPTabz[K,void,Z,z];
                                  key: K): bool {.inline.} =
  t.getPut(i, key) do: t.cell(i).key = key; result = true
  do: t.cell(i).key = key; result = false

proc mgetOrPut*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K;
                                    val: V): var V {.inline.} =
  t.getPut(i, key) do: result = t.cell(i).val
  do: (let c = t.cell(i); c.key = key; c.val = val; result = c.val)

proc mgetOrPut*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K; val: V;
                                    had: var bool): var V {.inline.} =
  t.getPut(i, key) do:
    had=true ; result = t.cell(i).val
  do:
    had=false; (let c = t.cell(i)); c.key = key; c.val = val
    result = c.val

template editOrInit*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K;
                                         v, found, missing: untyped): untyped =
  mixin cell, getPut
  getPut(t, i, key) do:
    var v {.inject.} = t.cell(i).val.addr
    found
  do:
    let c = t.cell(i)
    c.key = key
    var v {.inject.} = c.val.addr
    missing

template withIt*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; k: K;
                                     edit, init): untyped =
  ## Provide value variable `it` corresponding to key `k` in both bodies that
  ## represents the value found|allocated in the table.
  mixin cell, getPut
  template it: var V {.inject.} = t.cell(i).val
  getPut(t, i, k):
    edit
  do:
    t.cell(i).key = k
    init

template getItOrFail*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; k: K;
                                          found, missing): untyped =
  ## Provide value `it` corresponding to key `k` in `found` or do `missing`.
  mixin cell, getPut
  template it: var V {.inject.} = t.cell(i).val
  getPut(t, i, k):
    found
  do:
    t.cell(i).key = k
    missing

template doAdd(postAdd: untyped) {.dirty.} =
  mixin getCap
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
  t.cell(k).key = key
  postAdd

proc add*[K,Z;z:static int](t: var LPTabz[K,void,Z,z]; key: K) {.inline.} =
  doAdd: discard

proc add*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K,
                              val: V) {.inline.} =
  doAdd: t.cell(k).val = val

proc missingOrExcl*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K): bool =
  t.popRet(i, key) do: t.rawDel i
  do: result = true

proc take*[K,Z;z:static int](t: var LPTabz[K,void,Z,z]; key: var K): bool =
  t.popRet(i, key) do: key = move(t.cell(i).key); t.rawDel i; result = true
  do: discard

proc take*[K,V: not void,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K;
                                         val: var V): bool {.inline.} =
  t.popRet(i, key) do:
    val = move(t.cell(i).val)
    t.rawDel i
    result = true
  do: discard

proc pop*[K,Z;z:static int](t: var LPTabz[K,void,Z,z]): K {.inline.} =
  if t.len == 0: raise newException(KeyError, "collection is empty")
  when Z is K or Z is void:
    for i in 0 ..< t.getCap:
      if t.isUsed(i):             # Slows down as a set empties (as does regular
        result = t.data[i].key    #..iteration).  Could keep a running "min ix"
        t.rawDel i                #..updated by ins/del ops. XXX Performance
        return                    # Early exit => cheaper than it may look
  else:
    result = t.data[^1].key       # seq pop avoids O(len) shift on data
    t.rawDel t.rawGetLast         # does the s.data.pop internally

proc pop*[K,V: not void,Z;z:static int](t: var LPTabz[K,V,Z,z]):
    (K, V) {.inline.} =
  if t.len == 0: raise newException(KeyError, "collection is empty")
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

proc editKey*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]; old, new: K) =
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

proc nthKey*[K,Z;z:static int](t: LPTabz[K,void,Z,z]; n: int): K {.inline.} =
  ## Insertion-ordered multisets support 0-origin nth-in-order key.
  when Z is K or Z is void:
    assert false, "hash-order multisets do not support ins-order query"
  else:
    t.data[n].key

proc nthPair*[K,V:not void,Z;z:static int](t: LPTabz[K,V,Z,z]; n: int): (K, V) =
  ## Insertion-ordered tables support 0-origin nth-in-order pair.
  when Z is K or Z is void:
    assert false, "hash-order multitables do not support ins-order query"
  else:
    (t.data[n].key, t.data[n].val)

proc nthPair*[K,V:not void,Z;z:static int](t: var LPTabz[K,V,Z,z];
                                           n: int): (K, ptr V) {.inline.} =
  ## Insertion-ordered tables support 0-origin nth-in-order pair w/editable val.
  when Z is K or Z is void:
    assert false, "hash-order multitables do not support ins-order query"
  else:
    (t.data[n].key, t.data[n].val.addr)

proc clear*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]) =
  if t.getCap == 0: return
  when Z is K or Z is void:
    t.count = 0
    when Z is void:
      zeroMem t.data[0].addr, t.getCap * t.data[0].sizeof
    else:
      if K(z) == K(0):
        zeroMem t.data[0].addr, t.getCap * t.data[0].sizeof
      else:
        for i in 0 ..< t.getCap: t.unUse i
  else:
    t.idx.clear
    t.data.setLen 0

iterator cells[K,V,Z;z:static int](s: LPTabz[K,V,Z,z]): HCell[K,V,Z,z] =
  let L = s.len
  if s.len > 0:
    for cell in s.data:
      assert(s.len == L, "cannot edit while iterating")
      when Z is K or Z is void:
        if not cell.isUsed: continue
      yield cell

iterator mcells[K,V,Z;z:static int](s: var LPTabz[K,V,Z,z]):ptr HCell[K,V,Z,z]=
  let L = s.len
  if s.len > 0:
    for i in 0 ..< s.data.len:
      assert(s.len == L, "cannot edit while iterating")
      when Z is K or Z is void:
        if not s.isUsed(i): continue
      yield s.data[i].addr

iterator items*[K,Z;z:static int](s: LPTabz[K,void,Z,z]): K =
  for cell in s.cells: yield cell.key

iterator mitems*[K,Z;z:static int](s: var LPTabz[K,void,Z,z]): var K =
  for cell in s.mcells: yield cell.key

iterator hcodes*[K,Z;z:static int](s: LPTabz[K,void,Z,z]): (int, Hash) =
  when Z is void:
    for cell in s.cells: yield (i, cell.hcode)
  else:
    for i in 0 ..< s.getCap:
      if s.isUsed(i): yield (i, s.hash(i))

iterator allItems*[K,Z;z:static int](s: LPTabz[K,void,Z,z]; key: K): K =
  let L = s.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(s.hashHc(hc0), s.high):
    assert(s.len == L, "cannot edit while iterating")
    if not s.isUsed(i): break
    if s.equal(i, key, hc0): yield s.key(i)

# Optimizable, but wraparound makes tricky.  Low index slots may be follow-on of
# high end collision cluster.  Could start iterators after 1st unused slot, but
# that slot could be the very last one & don't want *all* to be slower.  Etc.
proc numItems*[K,Z;z:static int](t: LPTabz[K,void,Z,z]; key: K): int{.inline.}=
  for key in t.allItems(key): result.inc

iterator numItems*[K,Z;z:static int](t: LPTabz[K,void,Z,z]): (K, int) =
  for key in t.keys: yield (key, t.numItems(key))

iterator pairs*[K,Z;z:static int](t: LPTabz[K,void,Z,z]): (int, K) =
  var j = 0
  for cell in t.cells: yield (j, cell.key); j.inc

iterator pairs*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): (K,V) =
  for cell in t.cells: yield (cell.key, cell.val)

iterator mpairs*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]): (K, var V) =
  for cell in t.mcells: yield (cell.key, cell.val)

iterator keys*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): K =
  for cell in t.cells: yield cell.key

iterator values*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]): V =
  for cell in t.cells: yield cell.val

iterator mvalues*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z]): var V =
  for cell in t.mcells: yield cell.val

iterator allValues*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K): V =
  let L = t.len
  let hc0 = hash0[K,Z](key)
  for i in probeSeq(t.hashHc(hc0), t.high):
    assert(t.len == L, "cannot change a tab while iterating over it")
    if not t.isUsed(i): break
    if t.equal(i, key, hc0): yield t.cell(i).val

iterator allValues*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; vals:var seq[V]): K=
  for key in t.keys:
    vals.setLen 0
    for val in t.allValues(key): vals.add val
    yield key

proc allValues*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z]; key: K,
                                    vals: var seq[V]): bool {.inline.} =
  vals.setLen 0
  for val in t.allValues(key): vals.add val
  vals.len > 0

proc debugDump*[K,V,Z;z:static int](s: LPTabz[K,V,Z,z]; label="") =
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

proc pop*[K,Z;z:static int](s: var LPTabz[K,void,Z,z]; key: var K): bool =
  s.take key

proc incl*[K,Z;z:static int](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.containsOrIncl(elt)

proc excl*[K,Z;z:static int](s: var LPTabz[K,void,Z,z], elt: K) {.inline.} =
  discard s.missingOrExcl(elt)

proc incl*[K,Z;z:static int](s: var LPTabz[K,void,Z,z],
                             other: LPTabz[K,void,Z,z]) =
  for elt in other: s.incl elt

proc excl*[K,Z;z:static int](s: var LPTabz[K,void,Z,z],
                             other: LPTabz[K,void,Z,z]) =
  for elt in other: s.excl elt

proc card*[K,Z;z:static int](s: LPTabz[K,void,Z,z]): int {.inline.} = s.len

proc union*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  result = s1
  result.incl s2

proc intersection*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result.init min(s1.len, s2.len)
  for elt in s1:
    if elt in s2: result.incl elt

proc difference*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result.init
  for elt in s1:
    if elt notin s2: result.incl elt

proc symmetricDifference*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]):
    LPTabz[K,void,Z,z] =
  result = s1
  for item in s2:
    if result.containsOrIncl(item): result.excl item

proc `+`*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  s1.union s2
proc `*`*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  s1.intersection s2
proc `-`*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  s1.difference s2
proc `-+-`*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): LPTabz[K,void,Z,z] =
  s1.symmetricDifference s2

proc disjoint*[K,Z;z:static int](s1, s2: LPTabz[K,void,Z,z]): bool =
  for item in s1:
    if item in s2: return
  result = true

proc `<=`*[K,Z;z:static int](s, t: LPTabz[K,void,Z,z]): bool =
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

proc `<`*[K,Z;z:static int](s, t: LPTabz[K,void,Z,z]): bool =
  s.len != t.len and s <= t

proc `==`*[K,Z;z:static int](s, t: LPTabz[K,void,Z,z]): bool =
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

proc map*[K,A,Z;z:static int](data: LPTabz[K,void,Z,z], op: proc (x: K):
    A {.closure.}): LPTabz[A,void,Z,z] =
  result.init data.len
  for item in data: result.incl op(item)

proc toLPTabz*[K;V:void;Z;z:static int](keys: openArray[K],
                                        dups=false): LPTabz[K,V,Z,z] =
  result.init keys.len
  if dups:
    for item in keys: result.add item   # append; multi-set
  else:
    for item in keys: result.incl item  # replace; unique set

proc `$`*[K,Z;z:static int](s: LPTabz[K,void,Z,z]): string =
  if s.len == 0: return "{}"
  result = "{"
  for key in s:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
  result.add("}")

proc hash*[K,Z;z:static int](s: LPTabz[K,void,Z,z]): Hash =
  for i, hc in s.hcodes: result = result xor hc
  result = !$result  #Important to use a COMMUTATIVE combiner above

proc depthStats*[K,V,Z;z:static int](s: LPTabz[K,V,Z,z]):
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

proc toLPTabz*[K;V: not void,Z;z:static int](pairs: openArray[(K,V)],
                                             dups=false): LPTabz[K,V,Z,z] =
  result.init pairs.len
  if dups:
    for key, val in items(pairs): result.add(key, val) # append; multi-table
  else:
    for key, val in items(pairs): result[key] = val    # replace

proc `$`*[K,V: not void,Z;z:static int](t: LPTabz[K,V,Z,z]): string =
  if t.len == 0: return "{:}"
  result = "{"
  for key, val in t:
    if result.len > 1: result.add(", ")
    result.addQuoted(key)
    result.add(": ")
    result.addQuoted(val)
  result.add("}")

proc pop*[K,V: not void,Z;z:static int](t: var LPTabz[K,V,Z,z]; key: K;
                                        val: var V): bool {.inline.} =
  t.take key, val

template withValue*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], key: K;
                                        value, found: untyped): untyped =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    found

template withValue*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], key: K; value,
                                        found, missing: untyped): untyped =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0:
    var value {.inject.} = t.cell(i).val.addr
    found
  else: missing

proc `[]`*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z], key: K): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: raiseNotFound(key)

proc `[]`*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z],
                               key: K): var V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  if i >= 0: result = t.cell(i).val
  else: result = t.cell(0).val; raiseNotFound(key)

proc `[]=`*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z],
                                 key: K, val: V) {.inline.} =
  t.getPut(i,key) do: t.cell(i).val = val #Replace 1st FOUND elt in multimap|put
  do: (let c = t.cell(i); c.key = key; c.val = val)

proc `{}`*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z],
                               key: K): V {.inline.} = t[key]
proc `{}`*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z],
                               key: K): var V {.inline.} = t[key]
proc `{}=`*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], key: K;
                                val: V) {.inline.} = t[key] = val

proc hasKey*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z],
                                 key: K): bool {.inline.} = key in t

proc hasKeyOrPut*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z],
                                      key: K, val: V): bool {.inline.} =
  discard t.mgetOrPut(key, val, result)

proc getOrDefault*[K,V,Z;z:static int](t: LPTabz[K,V,Z,z], key: K,
                                       def=default(V)): V {.inline.} =
  mixin rawGet
  let i = t.rawGet(key)
  result = if i >= 0: t.cell(i).val else: def

proc del*[K,V,Z;z:static int](t: var LPTabz[K,V,Z,z], key: K) {.inline.} =
  ## delete one key `key` from `t`; If you want to know if it was present then
  ## use `missingOrExcl`, `take`, or `pop` instead.
  discard t.missingOrExcl(key)

proc `==`*[K,V: not void,Z;z:static int](x, y: LPTabz[K,V,Z,z]): bool =
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

proc indexBy*[A, K,V,Z;z:static int](collection: A,
                                     index: proc(x: V): K): LPTabz[K,V,Z,z] =
  result.init
  for item in collection: result[index(item)] = item

# 3 defs to maybe obsolete `CountTable` or let it be a type alias.
proc inc*[K,V,Z;z:static int](c: var LPTabz[K,V,Z,z], key: K,
                              amount: V=1) {.inline.} =
  ## Increment a `key` counter in table `t` by amount.
  c.editOrInit(key, val):
    val[] += amount
    if val[] == 0: c.del key
  do:
    val[] = amount

proc merge*[K,V,Z;z:static int](c: var LPTabz[K,V,Z,z], b: LPTabz[K,V,Z,z]) =
  ## Merge values from CountTable/histogram `b` into histogram `c`.
  for key, val in b: c.inc(key, val)

iterator topByVal*[K,V,Z;z:static int](c: LPTabz[K,V,Z,z], n=10,
                                       min=V.low, order=Cheap): (K, V) =
  ## Iterate from smallest to largest over biggest `n` items by value in `c`.
  ## `order` can be `Cheap`, `Ascending`, or `Descending`.
  var t = initTopK[(V,K)](n)
  for k, v in lptabz.pairs(c): (if v >= min: t.push (v, k))
  for e in topk.maybeOrdered(t, order): yield (e[1], e[0])

iterator mostCommon*[K](xs: openArray[K], n=10): (K, int) =
  ## Iterate over (`n` most common values in `xs`, their counts) tuples.
  var cnt: LPTabz[K,int,void,0]
  for x in xs: cnt.inc x
  for tup in cnt.topByVal(n): yield tup

# Specializations;  Q: add ILSet/OLSet/etc. for back compat..?
proc initLPSetz*[K,Z;z:static int](initialSize=lpInitialSize, numer=lpNumer,
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
