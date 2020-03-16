## This module provides a memory optimized ``seq[uint]`` for a user-given range
## of numbers (by default its own length).  E.g., if the range is 0..63, it uses
## just 6 bits per number (plus rounding error).  In the best cases this allows
## us to pack numbers up to about 2x (e.g., 64/33) more densely.  The public API
## uses only the widest type, usually a 64-bit unsigned integer.  To store n
## indices (default mode) takes only n*ceil(lg(n)) bits.  24*2**24/8 is only 48
## MiB and circa 2020 L3 caches have become that large.  Full 21-bit unicode can
## fit in 2.78 MiB and 14..17 bit spaces fit in typical L1/L2.
##
## Using the wide type for backing store and limiting the design to hold only
## numbers up to said wide type ensures <= 2 consecutive backing items are
## needed to form any given number.  We also assume the almost universal pattern
## that the number of bits in that widest type is a power of 2 like 16, 32, 64.

import bitop #, strformat
const iBit = 8 * sizeof(int)
const iShf = lgPow2(iBit)
const iMsk = (1 shl iShf) - 1

type
  SeqUint* = object
    data: seq[uint]  # Backing store, probably 64-bit ints
    len: uint
    bits: int8

proc roundUp(sz: int): int {.inline.} =
  let m = sz and iMsk
  if m == 0: return sz
  return sz + iBit - m

proc low*(s: SeqUint): int {.inline.} = 0

proc len*(s: SeqUint): int {.inline.} = int(s.len)

proc high*(s: SeqUint): int {.inline.} = int(s.len) - 1

#TODO Does not yet support resize, setLen, add, etc.
#TODO Could potentially optimize 8,16,32 bit cases with CPU supported types
proc init*(s: var SeqUint, initialSize=0, numBound=0) {.inline.} =
  let bits = if   numBound    > 0: lg(numBound)
             elif initialSize > 0: lg(initialSize)
             else: 0
  let bitsz = initialSize * bits
  if bitsz > 0:
    s.data.setLen (roundUp(bitsz) shr iShf)
    s.len  = uint(initialSize)
    s.bits = bits.int8
# echo "bits: ", s.bits, " bitsz: ", bitsz, " data.len: ", s.data.len

proc initSeqUint*(initialSize=0, numBound=0): SeqUint {.inline.} =
  result.init(initialSize, numBound)

# Consider storing 3 bit numbers packed into 8 bit words big-endian-wise like:
#   indices for trad. R2Left ops:  76543210             76543210  76543210
# The layout can be either A) m=1 [....210.] OR B) m=7 [0.......][......21].
# Goes to bit-algebra `(w shr m) msk and` OR `(w1 and 3) shl 2 or (w0 shr m)`
# where `m == bitix % 8` is the modulus of low order bit index relative to wdsz.
proc `[]`*(s: SeqUint, i: uint): uint {.inline.} =
  if i >= s.len:
    raise newException(IndexError, formatErrorIndexBound(i, s.len))
  let sbits  = uint(s.bits)
  let bitix  = i * sbits
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
proc `[]=`*(s: var SeqUint, i: uint, x: uint) {.inline.} =
  if i >= s.len:
    raise newException(IndexError, formatErrorIndexBound(i, s.len))
  let sbits  = uint(s.bits)
  let bitix  = i * sbits
  let wdix   = bitix shr iShf
  let wdmod  = bitix and iMsk
  let bitend = wdmod + sbits
  if bitend <= iBit:
    let wd = s.data[wdix]
    let hiM   = (not 0'u) shr bitend shl bitend
    let mMask = (1'u shl wdmod) - 1
#   echo &"before: s.data[{wdix}] = {s.data[wdix]:#022o}"
    s.data[wdix] = (wd and hiM) or (x shl wdmod) or (wd and mMask)
#   echo &"after;\n  hiM: {hiM:#022o}\n  mMask: {mMask:#024o}"
#   echo &"  up|loM: {hiM or mMask:#022o}"
#   echo &"  data[{wdix}] = {s.data[wdix]:#022o}"
  else:
    let w0bit = iBit - wdmod
    let oFlow = sbits - w0bit
    let w0    = s.data[wdix]
    let w1    = s.data[wdix + 1]
    let oMask = (1'u shl oFlow) - 1
    let loM   = (1'u shl wdmod) - 1
    let cMask = (1'u shl (iBit - wdmod)) - 1
#   echo &"BEFORE: s.data[{wdix}] = {s.data[wdix]:#022o}"
#   echo &"        s.data[{wdix+1}] = {s.data[wdix+1]:#022o}"
    s.data[wdix]   = ((x and cMask) shl wdmod) or (w0 and loM)
    s.data[wdix+1] = (w1 and not oMask) or (x shr w0bit)
#   echo &"AFTER: s.data[{wdix}] = {s.data[wdix]:#022o}"
#   echo &"       s.data[{wdix+1}] = {s.data[wdix+1]:#022o}"

proc `$`*(s: var SeqUint): string =
  result = "["
  for i in 0'u ..< s.len:
    result.add (if i < s.len-1: $s[i] & ", " else: $s[i])
  result.add "]"

when isMainModule:
  var s1 = initSeqUint(16)
  for i in 0'u ..< s1.len:      # Single big word, even small per big, fwd order
    let n = i
    s1[i] = n
    if s1[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s1[i], " BACK"

  var s2 = initSeqUint(44, numBound=16)
  for i in 0'u ..< s2.len:      # Three big words, even small per big, fwd order
    let n = i and 15
    s2[i] = n
    if s2[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s2[i], " BACK"

  var s3 = initSeqUint(128, 8)
  for i in 0'u ..< s3.len:      # Six big words, uneven small per big, fwd
    let n = i and 7
    s3[i] = n
    if s3[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s3[i], " BACK"

  var s4 = initSeqUint(64*13, numBound=32)
  for i in 0'u ..< s4.len:      # 65 big words, 5-bit nums, pseudo-rand vals
    let n = (i * 19) and 31
    s4[i] = n
    if s4[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s4[i], " BACK"

  # Now all the same as above but looping high to low
  var s5 = initSeqUint(16)
  for i in countdown(s5.len - 1, 0'u):
    let n = i
    s5[i] = n
    if s5[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s5[i], " BACK"

  var s6 = initSeqUint(44, numBound=16)
  for i in countdown(s6.len - 1, 0'u):
    let n = i and 15
    s6[i] = n
    if s6[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s6[i], " BACK"

  var s7 = initSeqUint(128, 8)
  for i in countdown(s7.len - 1, 0'u):
    let n = i and 7
    s7[i] = n
    if s7[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s7[i], " BACK"

  var s8 = initSeqUint(64*13, numBound=32)
  for i in countdown(s8.len - 1, 0'u):
    let n = (i * 19) and 31
    s8[i] = n
    if s8[i] != n: echo "i: ", i, " SET ", n, " BUT GOT ", s8[i], " BACK"
