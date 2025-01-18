## This module provides a memory optimized `seq[uint]` for a user-given range of
## numbers (by default its own initial length).  E.g., if the range is 0..7, it
## uses just 3 bits per number (plus rounding error).  Other pithy descriptions
## are "the array version of bit fields" | "the matrix version of bit vectors".
##
## In the best case, this allows packing numbers 8x (e.g., 8/1) more densely
## than a "next biggest CPU int rounded" approach (like 8,16,32,64).  The public
## API uses `uint`, usually a 64-bit unsigned integer.
##
## To store `n` indices from `0..n-1` takes `n*ceil(lg(n))` bits.  E.g., circa
## 2020 L3 CPU caches have become large enough to support any permutation of
## 0..2^24-1 since (24*2**24/8 = 48 MiB).
##
## Using the widest type for backing store and limiting the design to hold only
## numbers up to said wide type ensures <= 2 consecutive backing items are ever
## needed to access a given number.
##
## While dynamically growing a `SeqUint` works, changing *element size* doesn't.
## So, callers must `t=initSeqUint(s.len, 1 shl (s.bits+1))` & copy as needed.

# We also assume the near universal pattern that the widest type is a power of 2
# number of bits like 16, 32, 64.  { ISO/IEC JTC1 SC22 WG14 n2472 makes sizes of
# exact-bit ints next po2 for C anyway. }
#
# NOTE: The core of this module is just `[]` & `[]=` which can *very likely* be
# optimized in non-CPU-portable ways.  PRs for such are not only welcome, but
# actively solicited.  This is a space optimization almost all know about yet
# few use (since impls are just not handy or may be slow).  Thus it is not a bad
# choice for Nim stdlib inclusion, especially with per CPU speed forks.

import bitop
const iBit = 8 * sizeof(int)
const iShf = lgPow2(iBit)
const iMsk = (1 shl iShf) - 1

type
  SeqUint* = object   ## A space-optimized `seq[uint]`
    data: seq[uint]   # Backing store, probably 64-bit ints
    len: int          # number of elements occupied
    bits: int8        # size of elements in bits

proc roundUp(sz: int): int {.inline.} = # number of int-sized words for sz elts
  let m = sz and iMsk
  if m == 0: return sz
  return sz + iBit - m

proc bits*(s: SeqUint): int {.inline.} = int(s.bits)

proc low*(s: SeqUint): int {.inline.} = 0

proc addr0*(s: SeqUint): pointer {.inline.} = s.data[0].unsafeAddr

proc len*(s: SeqUint): int {.inline.} = int(s.len)

proc high*(s: SeqUint): int {.inline.} = int(s.len) - 1

proc clear*(s: var SeqUint) {.inline.} =
  zeroMem s.data[0].addr, s.data.len * s.data[0].sizeof

#TODO Could easily optimize exact 8|16|32-bit cases with CPU supported types.
proc init*(s: var SeqUint, initialSize=0, numBound=0) {.inline.} =
  let bits = if   numBound    > 0: lg(numBound)
             elif initialSize > 0: lg(initialSize)
             else: 0
  let bitsz = initialSize * bits
  s.data.setLen if bitsz > 0: (roundUp(bitsz) shr iShf) else: 1
  s.len  = initialSize
  s.bits = bits.int8

proc initSeqUint*(initialSize=0, numBound=0): SeqUint {.inline.} =
  result.init(initialSize, numBound)

template BadIndex: untyped =
  when declared(IndexDefect): IndexDefect else: IndexError

# Consider storing 3 bit numbers packed into 8 bit words big-endian-wise like:
#   indices for trad. R2Left ops:  76543210             76543210  76543210
# The layout can be either A) m=1 [....210.] OR B) m=7 [0.......][......21].
# Goes to bit-algebra `(w shr m) and msk` OR `(w1 and 3) shl 2 or (w0 shr m)`
# where `m == bitix % 8` is the modulus of low order bit index relative to wdsz.
proc `[]`*(s: SeqUint, i: int|uint): uint {.inline.} =
  if int(i) >= s.len:
    raise newException(BadIndex(), formatErrorIndexBound(int(i), s.len))
  let sbits  = uint(s.bits)
  let bitix  = uint(i) * sbits
  let wdix   = bitix shr iShf
  let wdmod  = bitix and iMsk
  let bitend = wdmod + sbits
  if bitend <= iBit:
    result = (s.data[wdix] shr wdmod) and ((1'u shl sbits) - 1)
  else:
    let w0bit = iBit - wdmod
    let oFlow = sbits - w0bit
    let oMask = (1'u shl oFlow) - 1
    result = ((s.data[wdix+1]and oMask) shl w0bit) or (s.data[wdix] shr wdmod)

# Reconsider the above bit extraction diagram/example for bit deposit.  Here we
# update one or two words.  In A) the new value is one 3-way bitwise OR of two
# old & 1 new parts, e.g.: (old and 240)or(num shl 1)or(old and 1) or more
# generally (wd and hiM) or (num shl m) or (wd and mMask) where hiM is the
# complement of the m+3 shift and mMask the mask for m bits.  Case B) does two
# bitwise ORs stored to the pair of words.  The 1st goes to ((num and 1) shl 7)
# or (w0 and loM) while the 2nd to (w1 and not oMask) or (num shr w0bit).
proc `[]=`*(s: var SeqUint, i: int|uint, x: int|uint) {.inline.} =
  let x = uint(x) and ((1'u shl s.bits) - 1)
  if int(i) >= s.len:
    raise newException(BadIndex(), formatErrorIndexBound(i, s.len))
  let sbits  = uint(s.bits)
  let bitix  = uint(i) * sbits
  let wdix   = bitix shr iShf
  let wdmod  = bitix and iMsk
  let bitend = wdmod + sbits
  if bitend <= iBit:
    let wd = s.data[wdix]
    let hiM = if bitend == iBit: 0'u else: (not 0'u) shr bitend shl bitend
    let mMask = (1'u shl wdmod) - 1
    s.data[wdix] = (wd and hiM) or (x shl wdmod) or (wd and mMask)
  else:
    let w0bit = iBit - wdmod
    let oFlow = sbits - w0bit
    let w0    = s.data[wdix]
    let w1    = s.data[wdix + 1]
    let oMask = (1'u shl oFlow) - 1
    let loM   = (1'u shl wdmod) - 1
    let cMask = (1'u shl (iBit - wdmod)) - 1
    s.data[wdix]   = ((x and cMask) shl wdmod) or (w0 and loM)
    s.data[wdix+1] = (w1 and not oMask) or (x shr w0bit)

proc setLen*(s: var SeqUint, size: int) {.inline.} =
  let bitsz = size * s.bits
  s.data.setLen (roundUp(bitsz) shr iShf)
  s.len = size

proc add*(s: var SeqUint, v: uint) {.inline.} =
  let i = s.len
  s.setLen i + 1
  s[i] = v

iterator items*(s: SeqUint): uint =
  for i in 0 ..< s.len: yield s[i]

iterator pairs*(s: SeqUint): (int, uint) =
  for i in 0 ..< s.len: yield (i, s[i])

proc `$`*(s: SeqUint): string =
  result = "["
  for i, v in s: result.add (if i < s.len - 1: $v & ", " else: $v)
  result.add "]"

when isMainModule:
  var s1 = initSeqUint(16)
  for i in 0 ..< s1.len:      # Single big word, even small per big, fwd order
    let n = uint(i)
    s1[i] = n
    if s1[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s1[i], " BACK"

  var s2 = initSeqUint(44, numBound=16)
  for i in 0 ..< s2.len:      # Three big words, even small per big, fwd order
    let n = uint(i and 15)
    s2[i] = n
    if s2[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s2[i], " BACK"

  var s3 = initSeqUint(128, 8)
  for i in 0 ..< s3.len:      # Six big words, uneven small per big, fwd
    let n = uint(i and 7)
    s3[i] = n
    if s3[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s3[i], " BACK"

  var s4 = initSeqUint(64*13, numBound=32)
  for i in 0 ..< s4.len:      # 65 big words, 5-bit nums, pseudo-rand vals
    let n = uint((i * 19) and 31)
    s4[i] = n
    if s4[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s4[i], " BACK"

  # Now all the same as above but looping high to low
  var s5 = initSeqUint(16)
  for i in countdown(s5.len - 1, 0):
    let n = uint(i)
    s5[i] = n
    if s5[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s5[i], " BACK"

  var s6 = initSeqUint(44, numBound=16)
  for i in countdown(s6.len - 1, 0):
    let n = uint(i and 15)
    s6[i] = n
    if s6[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s6[i], " BACK"

  var s7 = initSeqUint(128, 8)
  for i in countdown(s7.len - 1, 0):
    let n = uint(i and 7)
    s7[i] = n
    if s7[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s7[i], " BACK"

  var s8 = initSeqUint(64*13, numBound=32)
  for i in countdown(s8.len - 1, 0):
    let n = uint((i * 19) and 31)
    s8[i] = n
    if s8[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s8[i], " BACK"

  var s9 = initSeqUint(0, numBound=32)
  for x in s8: s9.add x
  if $s9 != $s8: echo "grown seqUint != assigned"; echo s9; echo s8
  s9.setLen 0
  for x in s6: s9.add x
  if $s9 != $s6: echo "grown seqUint != assigned2"; echo s9; echo s6
