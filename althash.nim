## This module provides several alternative hashes to the Nim stdlib targeting
## high entropy in low order bits for ``and mask`` conversion to a table index.
## Activate via e.g., ``proc hash(x: K): Hash {.inline.} = hashRoMu1(x)``, where
## ``K`` is your integer key type.
##
## The fastest hash used here is a likely the multiply & rotate hash (lately
## called a "Fibonacci hash", after Knuth's golden ratio fascination.  Knuth
## immediately, in context, shows it works for any irrational number and then
## computer arithmetic is finite anyway).  This propagates entropy in key bits
## to the double-width product in a Pascal's Bionomial Triangle sort of pattern.
## Middle bits have the most key related entropy (there is even a PRNG method
## called "middle square" based on this pattern).  More specifically, consider
## the product of a pair of 2 digit decimal numbers: (a1*10+a0)*(b1*10+b0) =
## a1*b1*100 + (a1*b0+b0*a1)*10 + a0*b0.  This shows the "triangle" kind of
## pattern that makes the mixing strongest in the middle.
##
## The full product is easily accessed for half CPU register width and narrower
## numbers.  For the widest integer type, C/backend arithmetic rules only give
## the modulus|lower order bits of said product (x86/amd64 does make the upper
## word available in another register).  hashRoMu1 takes the simple portable way
## out and discards half of key entropy, just rotating down the more well mixed
## bits (bit reversal might be better, though expensive compared to rotr).
## Reducing hash codes to table address via ``shr`` is another way out.  Getting
## high order avalanche is more natural than the low order avalanche needed for
## ``and mask``, both may be https://burtleburtle.net/bob/hash/integer.html's
## "half avalanche".  Anyway, rotating is portably fast (often 1 cycle latency,
## 1-per cycle tput for an immediate constant number of bits).  Bit reversal is
## not so slow using a fully unrolled loop and 16 entry nibble lookup table.
## Different hashes will perform more/less well on different data.  So, we just
## provide a few here, and one is based upon highly non-linear bit reversal.
##
## The strongest hash is ``hashWangYi1`` which passes all of SMHasher's entropy
## tests and so is the default Hash-rehasher.  This uses the primitive of the
## xor or the high and low parts of a double-width product of a salt with a key.
## The xor blends the well mixed low order bits of the high output product word
## with the less well mixed low order bits of the low output product word,
## making "mostly level" mixing across all bits of the hash.  ``hashWangYi1``
## takes two rounds of such mixing to achieve avalanche.  It may be possible to
## "nearly pass" in only one round.  ``hashWY0`` is one attempt at that, but the
## salt may need to be re-optimized to have any hope of doing well on SMHasher.
##
## (Incidentally, most "fast in GB/s" hashes are far too slow for just one int.
## Even assessing them that way for lookup tables is misleading.  You want time
## =~ a + b*nBytes (at least) where a & b maybe come from a linear regression.
## Just 1/b tells you little, especially for integer keys where ``a`` dominates,
## although short string hashes can be similarly misleading.  Anyway, at least
## two summary numbers are desired, not one.  Whole curves are even better.)

import std/hashes, bitop    # For the Hash type and system `hash()`es
export Hash, `!$`, hash

proc hashRoMu1*[T: int8|uint8](x: T): Hash {.inline.}  =
  Hash(rotateRightBits(uint64(uint16(x) * 0xd3'u16), 7))

proc hashRoMu1*[T: int16|uint16](x: T): Hash {.inline.}  =
  Hash(rotateRightBits(uint64(uint32(x) * 0xd383'u32), 15))

proc hashRoMu1*[T: int32|uint32](x: T): Hash {.inline.} =
  Hash(rotateRightBits(uint64(x) * 0xd3833e81'u64, 31))

proc hashRoMu1*(x: int64|uint64|Hash): Hash {.inline.} =
  ## 1-hop of Romul-DuoJr using x as seed
  Hash(rotateLeftBits(x * 15241094284759029579'u64, 27))

proc hashRoMu2*(x: int64|uint64|Hash): Hash {.inline.} =
  ## Just the cross terms of 128-bit whole product
  # (h*s + l) * (a*s + b) = h*a*s*s + h*b*s + l*a*s + l*b.  Constants taken
  # from roMuDuoJr;  Could probably be tuned to get near avalanche.
  let lo = uint64(x) and 0xFFFFFFFF'u64
  let hi = uint64(x) shr 32
  Hash(rotateLeftBits(0xD3833E80'u64 * lo  +  hi * 0x4F4C574B'u64, 27))

proc hiXorLoFallback32(a, b: uint64): uint64 {.inline, used.} =
  let # Fall back in 32-bit arithmetic
    adig = [ uint16(a and 0xFFFF'u64), uint16((a shr 16) and 0xFFFF'u64),
             uint16((a shr 32) and 0xFFFF'u64), uint16(a shr 48) ]
    bdig = [ uint16(b and 0xFFFF'u64), uint16((b shr 16) and 0xFFFF'u64),
             uint16((b shr 32) and 0xFFFF'u64), uint16(b shr 48) ]
  var prod: array[8, uint16]
  for bi in 0..3:                         # for all digits in b
    var carry = 0'u16
    for ai in 0..3:                       # for all digits in a
      let j   = ai + bi
      let dig = uint32(prod[j])+uint32(carry)+uint32(adig[ai])*uint32(bdig[bi])
      carry   = uint16(dig shr 16)
      prod[j] = uint16(dig and 65535)
    prod[bi + 4] = carry                  # last digit comes from final carry
  result =  uint64(prod[0] xor prod[4]) or
           (uint64(prod[1] xor prod[5]) shl 16) or
           (uint64(prod[2] xor prod[6]) shl 32) or
           (uint64(prod[3] xor prod[7]) shl 48)

proc hiXorLoFallback64(a, b: uint64): uint64 {.inline.} =
  let # Fall back in 64-bit arithmetic
    aH = a shr 32
    aL = a and 0xFFFFFFFF'u64
    bH = b shr 32
    bL = b and 0xFFFFFFFF'u64
    rHH = aH * bH
    rHL = aH * bL
    rLH = aL * bH
    rLL = aL * bL
    t = rLL + (rHL shl 32)
  var c = if t < rLL: 1'u64 else: 0'u64
  let lo = t + (rLH shl 32)
  c += (if lo < t: 1'u64 else: 0'u64)
  let hi = rHH + (rHL shr 32) + (rLH shr 32) + c
  result = hi xor lo

proc hiXorLo(a, b: uint64): uint64 {.inline.} =
  # Xor of high & low 8B of full 16B product
  when nimvm:
    result = hiXorLoFallback64(a, b) # `result =` is necessary here.
  else:
    when Hash.sizeof < 8 or defined(useHiXorLoFallback32):
      result = hiXorLoFallback32(a, b)
    elif defined(useHiXorLoFallback64):
      result = hiXorLoFallback64(a, b)
    elif defined(gcc) or defined(llvm_gcc) or defined(clang):
      {.emit: """__uint128_t r = a; r *= b; `result` = (r >> 64) ^ r;""".}
    elif defined(windows) and not defined(tcc):
      {.emit: """a = _umul128(a, b, &b); `result` = a ^ b;""".}
    else:
      result = hiXorLoFallback64(a, b)

proc hashWangYi1*(x: int64|uint64|Hash): Hash {.inline.} =
  ## Wang Yi's hash_v1 for 8B int.  https://github.com/rurban/smhasher has more
  ## details.  This passed all scrambling tests in Spring 2019 and is simple.
  ## NOTE: It's ok to define ``proc(x: int16): Hash = hashWangYi1(Hash(x))``.
  const P0 = 0xa0761d6478bd642f'u64
  const P1 = 0xe7037ed1a0b428db'u64
  const P5x8 = 0xeb44accab455d165'u64 xor 8'u64
  Hash(hiXorLo(hiXorLo(P0, uint64(x) xor P1), P5x8))

proc hashWY0*(x: int64|uint64|Hash): Hash {.inline.} =
  ## A slightly simplified/early version of Wang Yi's hash for 8B ints.
  ## Faster, but less scrambling.  Definitely has some easy weak spots.
  const P0 = 0xa0761d6478bd642f'u64
  const P1 = 0xe7037ed1a0b428db'u64
  let x = uint64(x)
  Hash(hiXorLo(P0, x xor P1))

proc hashMoreMur*(x: int64|uint64|Hash): Hash {.inline.} =
  ## This is from https://github.com/tommyettinger
  var x = uint64(x)
  x = x xor (x shr 27)
  x = x * 0x3C79AC492BA7B653'u64
  x = x xor (x shr 33)
  x = x * 0x1C69B3F74AC4AE35'u64
  x = x xor (x shr 27)
  result = Hash(x)

proc hashNASAM*(x: int64|uint64|Hash): Hash {.inline.} =
  # Pelle Evensen's NASAM the xor of an odd number of rotations of the same term
  # is invertible.  Here, one of the rotations is effectively a rotation by 0.
  var x = uint64(x)
  x = x xor (rotateRightBits(x, 25) xor rotateRightBits(x, 47))
  x = x * 0x9E6C63D0676A9A99'u64
  # All xorshifts in the same dir, even w/multiple xors & shifts, are invertible
  x = x xor (x shr 23) xor (x shr 51)
  x = x * 0x9E6D62D06F6A9A9B'u64
  x = x xor (x shr 23) xor (x shr 51)
  result = Hash(x)

proc hashRevFib*(x: int32|uint32): Hash {.inline.} =
  Hash(reverseBits(uint32(x) * 0xd3833e81'u64))

proc hashRevFib*(x: int64|uint64): Hash {.inline.} =
  Hash(reverseBits(uint64(x) * 15241094284759029579'u64))

proc secureSalt*(x: pointer): Hash {.inline.} =
  proc getrandom(buf: pointer, len: uint64, flags: cuint): csize {. importc:
    "getrandom", header: "sys/random.h" .}
  discard getrandom(result.addr, uint64(result.sizeof), cuint(0))

proc vmaddrSalt*(x: pointer): Hash {.inline.} =
  const roMuDuoJr = 15241094284759029579'u64  # selected to pair w/27 bit roll
  Hash(rotateLeftBits((cast[uint64](x) shr 3) * roMuDuoJr, 27))

proc zeroSalt*(x: pointer): Hash {.inline.} = 0

var getSalt* = vmaddrSalt

when int.sizeof == int64.sizeof:
  proc hashRoMu1*(x: int|uint): Hash {.inline.} = hashRoMu1(uint64(x))
  proc hashRevFib*(x: int|uint): Hash {.inline.} = hashRevFib(uint64(x))
else:
  proc hashRoMu1*(x: int|uint): Hash {.inline.} = hashRoMu1(uint32(x))
  proc hashRevFib*(x: int|uint): Hash {.inline.} = hashRevFib(uint32(x))

when defined(unstableHash):
  proc hash*(hsh, salt: Hash): Hash {.inline.} = hashWangYi1(hsh) xor Hash(salt)
else:
  proc hash*(hsh, salt: Hash): Hash {.inline.} = hashWangYi1(hsh)

when defined(hashDebug):
  template dbg*(x) = x
else:
  template dbg*(x) = discard

when isMainModule:
  echo hashWangYi1(456)
