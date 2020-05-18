## This module provides compact sets of non-0 B)-bit int keys L)inear Probing
## with optional Robin Hood re-organization via ``sequint``.  The only satellite
## data supported is a second b2-bit int (eg a counter).  Mnemonically, "BL" is
## also the start of "Bloom filter", an occasionally competing data structure.
#XXX Make all above statements true by adding in b2 field defaulted to zero len
#    and table ops; make Robin Hood re-org optional.  This is not ready to use!

import althash, sequint
type
  BLtab* = object  ## RobinHoodLP set of B-bit int keys w/small false pos. rate
    data: SeqUint     # number array
    count, mask: int  # count of entered slots

when defined(hashStats):
  template ifStats(x) = x
  var frDepth* = 0  ## Total search depth; like "getTime" v0=val;...; val-v0
else:
  template ifStats(x) = discard

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
    ifStats frDepth.inc
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

proc len*(s: BLtab): int {.inline.} = s.count

proc getCap*(s: BLtab): int {.inline.} = s.data.len

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

when isMainModule:
  import os, strutils
  let nTab = parseInt(paramStr(1))
  let mask = parseInt(paramStr(2))
  var s = initBLtab(nTab, mask)
  for i in 3 .. paramCount():
    let j = parseInt(paramStr(i))
    if j > 0:
      let k = hashNASAM(j) and mask
      if s.containsOrIncl(k): echo "had ", j
      else: echo "added ", (j, k)
    elif j < 0:
      let k = hashNASAM(-j) and mask
      if s.missingOrExcl(k): echo "did nothing"
      else: echo "removed ", (-j, k)
  echo s.data
  let ds = s.depths
  echo "hashLd: ", float(s.count)/float(s.getCap), " ", ds.len, " depths: ", ds
  echo paramCount()-2-s.len, '/', paramCount()-2, ". false pos. (if all +)"
# s.debugDump
# ./bltab $[1<<17] $[(1<<26) - 1] {1..$[3<<15]} |tail -n2
#   hashLd: 0.7494 19 depths: @[36541, 25619, 15310, 8917, 5087, 2965, 1708, 934, 486, 272, 174, 82, 56, 36, 17, 13, 5, 1, 2]
#   79/98304. false pos. (if all +); fpr=0.0008; 19*26/8=61.75 < 64B cache line.
# A Bloom filter is less space -1.44*lg.0008=14.8 bit/num (57% of 26 bits, 42.7%
# adjusting for 75% hashLd), BUT needs -lg .0008 = 10.3 hash funs ~10 line lds.
# *MANY* would pay a 1/.427=2.34X space increase to get a 10+X speed boost.
