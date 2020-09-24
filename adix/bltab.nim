## This module is a specialization of `BLTab` to the Bit-Level case where keys
## are 1..k-bit ints & values are 0..v-bit ints (`k+v<=8*int.sizeof`) using one
## `SeqUInt` as backing store.  (Mnemonically, "BL" is also the start of "BLoom
## Filter", a sometimes competing data structure.)  Users must give a number of
## bits for the key.  Bits for values and the sentinel key default to 0. `BLtab`
## tries to be similar otherwise to hash-ordered variants of BLTab multisets.

import althash, bitop, sequint, strutils#, memfiles, heapqueue
type
  BLtab* = object  ## RobinHoodLP set of B-bit int keys w/small false pos. rate
    data: SeqUint     # number array
    count: int        # count of entered slots
    k, v, numer, denom, minFree, growPow2, pow2: uint8 # size policy parameters
    rehash, robin: bool                           # Steal 2-bits from `salt`?
    salt: Hash                                    # ~unpredictable salt
    z: uint                                       # sentinel

var blInitialSize* = 2     ## default initial size aka capacity aka cap
var blNumer*       = 3'u8  ## default numerator for lg(n) probe depth limit
var blDenom*       = 1'u8  ## default denominator for lg(n) probe depth limit
var blMinFree*     = 1'u8  ## default min free slots; (>= 1)
var blGrowPow2*    = 1'u8  ## default growth power of 2; 1 means double
var blRobinHood*   = false ## default to Robin Hood re-org; auto-activated
var blRehash*      = false ## default hcode rehashing behavior; auto-activated

when defined(hashStats):   # Power user inspectable/zeroable stats.  These are
  template ifStats(x) = x  # all kind of like "times" - you v0=val;...; val-v0
  var blDepth*     = 0   ## Counts total search depth
  var blTooFull*   = 0   ## Counts resizes from minFree boundary
  var blTooDeep*   = 0   ## Counts resizes from deep probe sequences
  var blTooSparse* = 0   ## Counts skips of depth-triggered resize from sparsity
else:
  template ifStats(x) = discard
when defined(blWarn) or not defined(danger):
  var blWarn*    = stderr  ## Set to wherever you want warnings to go
  var blMaxWarn* = 10      ## Most warnings per program invocation
  var blWarnCnt  = 0       # Running counter of warnings issued

proc len*(s: BLtab): int {.inline.} = s.count
proc getCap*(s: BLtab): int {.inline.} = s.data.len

proc save*(t: BLTab, pathStub: string) = discard
proc load*(t: var BLTab, path: string) = discard
proc loadBLTab*(path: string): BLTab = discard
proc mmap*(t: var BLTab, path: string) = discard

proc pushUp(x: var SeqUint, i, n: int) {.inline.} =   # move n items up 1
  for j in countdown(i + n - 1, i): x[j+1] = x[j]

proc pullDown(x: var SeqUint, i, n: int) {.inline.} = # move n items down 1
  for j in countup(i, i + n - 1): x[j] = x[j+1]

proc isUsed(s: BLtab, i: int): bool {.inline.} = s.data[uint(i)] != 0

proc depth(i, hc, mask: Hash): Hash {.inline.} =
  let i = uint(i)
  let hc = uint(hc)
  let mask = uint(mask)
  Hash((i - hc) and mask)                 # Search depth of entry w/hcode @ix`i`

iterator probeSeq(hc, mask: Hash): int =
  var i: Hash = hc and mask               # Start w/home address
  while true:
    yield i
    i = (i + 1) and mask                  # Linear Probing

proc rawGet(s: BLtab; hc: Hash, d: var Hash): int {.inline.} =
  assert(s.data.len > 0, "Uninitialized BLtab")  # Ensure in *caller* not here
  var t {.noInit.}: int                          # Where to insert if missing
  for i in probeSeq(hc, s.data.high):
    t = i
    if not s.isUsed(i):
      break
    if d > depth(i, Hash(s.data[uint(i)]), s.data.high):
      break
    if s.data[i] == uint(hc):
      return i
    d.inc
    ifStats blDepth.inc
    if d == s.data.len:         # Handle fully saturated table case
      break
  result = -1 - t               # < 0 => MISSING and insert idx = -1 - result

proc rawGet(s: BLtab, hc: Hash): int {.inline.} =
  var d: Hash
  rawGet(s, hc, d)              # < 0 => MISSING and insert idx = -1 - result

proc depth(s: BLtab; hc: Hash): int {.inline.} =
  var d: Hash
  discard rawGet(s, hc, d)
  d

proc rawPut1(s: var BLtab, i: Hash; d: var int): int {.inline.} =
  result = i                          # Linear probe to first empty slot
  while s.isUsed(result):
    result = (result + 1) and s.data.high
    d.inc
    if d == s.data.len:
      raise newException(ResourceExhaustedError, "BLtab saturated")

proc rawPut2(s: var BLtab, i, j: Hash): int {.inline.} =
  if j > i:                           # No table wrap around; just shift up
    pushUp s.data, i, j - i
  elif j < i:                         # j wrapped to low indices
    pushUp s.data, 0, j
    s.data[0] = s.data[s.data.high]
    pushUp s.data, i, s.data.high - i
  result = i                          # j == i => already have space @i; done

proc rawDel(s: var BLtab, i: Hash) {.inline.} =
  let mask = s.data.high
  var k = i
  var j = (i + 1) and mask            # Find next empty|at home position entry
  while s.isUsed(j) and j != (int(s.data[j]) and mask):
    j = (j + 1) and mask
  if j > i + 1:                       # No table wrap around; just shift down
    pullDown s.data, i, j - 1 - i
    k = j - 1                         # Mark just-past-shift-block entry empty
  elif ((j + mask - i) and mask) > 0: # j wrapped to low indices; Did >0 j.inc
    pullDown s.data, i, mask - i
    s.data[mask] = s.data[0]
    pullDown s.data, 0, j - 1
    k = (j + mask) and mask           # [j-1 mod tabSz] is now empty
# else:                               # k == i is already home position
  s.data[k] = 0

proc init*(s: var BLtab, size, mask: int) {.inline.} =
  s.data  = initSeqUint(size, numBound=mask)
  s.count = 0

proc initBLtab*(size, mask: int): BLtab{.inline.} = result.init size, mask

proc contains*(s: BLtab, hc: Hash): bool {.inline.} =
  assert(s.data.len > 0, "Uninitialized BLtab")  # Ensure in *caller* not here
  s.rawGet(hc) >= 0

proc containsOrIncl*(s: var BLtab, hc: Hash): bool {.inline.} =
  assert(s.data.len > 0, "Uninitialized BLtab")  # Ensure in *caller* not here
  var d: Hash
  let i = s.rawGet(hc, d)
  if i < 0:
    var j = s.rawPut1(-1 - i, d)
    let k = s.rawPut2(-1 - i, j)                  # Maybe allocate a slot
    s.count.inc
    s.data[k] = hc
  else:
    result = true

proc missingOrExcl*(s: var BLtab, hc: Hash): bool {.inline.} =
  assert(s.data.len > 0, "Uninitialized BLtab")  # Ensure in *caller* not here
  let i = s.rawGet(hc)
  if i >= 0:
    s.data[i] = 0
    s.rawDel i
    s.count.dec
  else:
    return true

proc clear*(s: var BLtab) {.inline.} =
  s.data.clear
  s.count = 0

iterator items*(s: BLtab): Hash =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(i): yield Hash(s.data[i])

iterator pairs*(s: BLtab): tuple[a: int, b: Hash] =
  let L = s.len
  var j =  0
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    if s.isUsed(i): yield (j, Hash(s.data[i]))
    j.inc

proc depths*(s: BLtab): seq[int] =
  for elt in s:
    let d = s.depth(elt)
    if d >= result.len: result.setLen(d + 1)
    result[d] += 1

proc debugDump*(s: BLtab, label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  for i, cell in s.data:
    echo "i: ", i, " depth: ", if cell != 0: depth(i, int(s.data[i]), s.data.high) else: 0, " ", cell
