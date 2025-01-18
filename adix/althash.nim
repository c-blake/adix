## This module provides several alternative hashes to the Nim stdlib targeting
## high entropy in low order bits for `and mask` conversion to a table index.
## Activate via e.g., `proc hash(x: K): Hash {.inline.} = hashRoMu1(x)`, where
## `K` is your integer key type.
##
## The fastest hash used here is a likely the multiply & rotate hash (lately
## called a "Fibonacci hash", after Knuth's golden ratio fascination.  Knuth
## immediately, in context, shows it works for any irrational number and then
## computer arithmetic is finite anyway).  This propagates entropy in key bits
## to the double-width product in a Pascal's Binomial Triangle sort of pattern.
## Middle bits have the most key related entropy (there is even a PRNG method
## called "middle square" based on this pattern).  More specifically, consider
## the product of a pair of 2 digit decimal numbers: `(a1*10+a0)*(b1*10+b0) =
## a1*b1*100 + (a1*b0+b0*a1)*10 + a0*b0`. This shows the "triangle" kind of
## pattern that makes the mixing strongest in the middle.
##
## The full product is easily accessed for half CPU register width and narrower
## numbers.  For the widest integer type, C/backend arithmetic rules only give
## the modulus|lower order bits of said product (x86/amd64 does make the upper
## word available in another register).  hashRoMu1 takes the simple portable way
## out and discards half of key entropy, just rotating down the more well mixed
## bits (bit reversal might be better, though expensive compared to rotr).
## Reducing hash codes to table address via `shr` is another way out.  Getting
## high order avalanche is more natural than the low order avalanche needed for
## `and mask`, both may be https://burtleburtle.net/bob/hash/integer.html's
## "half avalanche".  Anyway, rotating is portably fast (often 1 cycle latency,
## 1-per cycle tput for an immediate constant number of bits).  Bit reversal is
## not so slow using a fully unrolled loop and 16 entry nibble lookup table.
## Different hashes will perform more/less well on different data.  So, we just
## provide a few here, and one is based upon highly non-linear bit reversal.
##
## A stronger hash is `hashWangYi1` which passes SMHasher's entropy tests, but
## "funnels" 64 inputs into about 62-bits of output entropy.  It is still the
## default Hash-rehasher since it is fast & tables with 2^62 entries are rare.
## WangYi1's primitive is the xor of the high & low parts of a double-width
## product of a salt with a key.  The xor blends well-mixed low order bits of
## the high output product word with less well-mixed low order bits of the low
## output product word, yielding "mostly level" mixing across all bits of the
## hash.  WangYi1 takes two rounds of such mixing to achieve avalanche.  It may
## be possible to "nearly pass" in only one round.  `hashWY0` is one attempt at
## that, but the salt may need to be re-optimized to have any hope of doing well
## on SMHasher.  This hash became the default `hash(int)` in Nim stdlib.
##
## There is a motley selection of other hashes here.  The strongest fast hash is
## Pelle Evensen's bijective (64-bits -> 64-bits) `hashNASAM`.  It's 2-3x slower
## than WangYi1 on some CPU architectures, but has no known statistical flaws.
##
## Incidentally, most "fast in GB/s" hashes are far too slow for just one int.
## Even assessing them so is misleading for lookup tables.  You want `time =~ a
## + b*nBytes` where a & b maybe come from line regressions.  `1/b` alone says
## too little, especially for integer keys where `a` dominates.  Short string
## hashes or `a` alone can similarly mislead.  TLDR, want >=2 "summary numbers"
## not one & whole curves are best.

import std/hashes, bitop    # For the Hash type and system `hash()`es
export Hash, `!$`, hash

proc hashRoMu1*(x: SomeOrdinal|Hash): Hash {.inline.} =
  ## 1-hop of Romul-DuoJr using x as seed
  cast[Hash](rotateLeftBits(uint64(x) * 15241094284759029579'u64, 27))

proc hashRoMu2*(x: SomeOrdinal|Hash): Hash {.inline.} =
  ## Just the cross terms of 128-bit whole product
  # (h*s + l) * (a*s + b) = h*a*s*s + h*b*s + l*a*s + l*b.  Constants taken
  # from roMuDuoJr;  Could probably be tuned to get near avalanche.
  let lo = uint64(x) and 0xFFFFFFFF'u64
  let hi = uint64(x) shr 32
  cast[Hash](rotateLeftBits(0xD3833E80'u64 * lo  +  hi * 0x4F4C574B'u64, 27))

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
    when Hash.sizeof < 8:
      result = hiXorLoFallback64(a, b)
    elif defined(gcc) or defined(llvm_gcc) or defined(clang):
      {.emit: """__uint128_t r = a; r *= b; `result` = (r >> 64) ^ r;""".}
    elif defined(windows) and not defined(tcc):
      proc umul128(a, b: uint64,
        c: ptr uint64): uint64 {.importc: "_umul128", header: "intrin.h".}
      var b = b
      let c = umul128(a, b, addr b)
      result = c xor b
    else:
      result = hiXorLoFallback64(a, b)

proc hashWangYi1*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## Wang Yi's hash_v1 for 8B int.  https://github.com/rurban/smhasher has more
  ## details.  This passed all scrambling tests in Spring 2019 and is simple.
  ## NOTE: It's ok to define ``proc(x:int16): Hash=hashWangYi1(cast[Hash](x))``.
  const P0  = 0xa0761d6478bd642f'u64
  const P1  = 0xe7037ed1a0b428db'u64
  const P58 = 0xeb44accab455d165'u64 xor 8'u64
  when nimvm:
    cast[Hash](hiXorLo(hiXorLo(P0, uint64(x) xor P1), P58))
  else:
    when defined(js):
      asm """
        if (typeof BigInt == 'undefined') {
          `result` = `x`; // For Node < 10.4, etc. we do the old identity hash
        } else {          // Otherwise we match the low 32-bits of C/C++ hash
          function hi_xor_lo_js(a, b) {
            const prod = BigInt(a) * BigInt(b);
            const mask = (BigInt(1) << BigInt(64)) - BigInt(1);
            return (prod >> BigInt(64)) ^ (prod & mask);
          }
          const P0  = BigInt(0xa0761d64)<<BigInt(32)|BigInt(0x78bd642f);
          const P1  = BigInt(0xe7037ed1)<<BigInt(32)|BigInt(0xa0b428db);
          const P58 = BigInt(0xeb44acca)<<BigInt(32)|BigInt(0xb455d165)^BigInt(8
          var res   = hi_xor_lo_js(hi_xor_lo_js(P0, BigInt(`x`) ^ P1), P58);
          `result`  = Number(res & ((BigInt(1) << BigInt(53)) - BigInt(1)));
        }"""
    else:
      cast[Hash](hiXorLo(hiXorLo(P0, uint64(x) xor P1), P58))

proc hashWY0*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## A slightly simplified/early version of Wang Yi's hash for 8B ints.
  ## Faster, but less scrambling.  Definitely has some easy weak spots.
  const P0 = 0xa0761d6478bd642f'u64
  const P1 = 0xe7037ed1a0b428db'u64
  let x = uint64(x)
  cast[Hash](hiXorLo(P0, x xor P1))

proc hashMoreMur*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## This is from https://github.com/tommyettinger
  var x = uint64(x)
  x = x xor (x shr 27)
  x = x * 0x3C79AC492BA7B653'u64
  x = x xor (x shr 33)
  x = x * 0x1C69B3F74AC4AE35'u64
  x = x xor (x shr 27)
  result = cast[Hash](x)

proc hashNASAM*[T: Ordinal|enum](x: T): Hash {.inline.} =
  # Pelle Evensen's NASAM the xor of an odd number of rotations of the same term
  # is invertible.  Here, one of the rotations is effectively a rotation by 0.
  var x = uint64(x)
  x = x xor (rotateRightBits(x, 25) xor rotateRightBits(x, 47))
  x = x * 0x9E6C63D0676A9A99'u64
  # All xorshifts in the same dir, even w/multiple xors & shifts, are invertible
  x = x xor (x shr 23) xor (x shr 51)
  x = x * 0x9E6D62D06F6A9A9B'u64
  x = x xor (x shr 23) xor (x shr 51)
  result = cast[Hash](x)

proc hashIdentity*[T: Ordinal|enum](x: T): Hash {.inline.} = cast[Hash](ord(x))

proc hashSplitMix*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## This is one hop of a PRNG.  For more information on the PRNG part see
  ## http://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html
  var z = uint64(x) + 0x9e3779b97f4a7c15'u64
  z = (z xor (z shr 30)) * 0xbf58476d1ce4e5b9'u64
  z = (z xor (z shr 27)) * 0x94d049bb133111eb'u64
  result = cast[Hash](z xor (z shr 31))

proc hashSplit64*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## https://nullprogram.com/blog/2018/07/31/
  var x = uint64(x)
  x = x xor (x shr 30)
  x *= 0xbf58476d1ce4e5b9'u64
  x = x xor (x shr 27)
  x *= 0x94d049bb133111eb'u64
  x = x xor (x shr 31)
  result = cast[Hash](x)

proc hashDegski*[T: Ordinal|enum](x: T): Hash {.inline.} =
  ## https://gist.github.com/degski/6e2069d6035ae04d5d6f64981c995ec2
  var x = uint64(x)
  x = x xor (x shr 32)
  x *= 0xd6e8feb86659fd93'u64
  x = x xor (x shr 32)
  x *= 0xd6e8feb86659fd93'u64
  x = x xor (x shr 32)
  result = cast[Hash](x)

proc hashRevFib*(x: int32|uint32): Hash {.inline.} =
  cast[Hash](reverseBits(uint32(x) * 0xd3833e81'u64))

proc hashRevFib*(x: int64|uint64): Hash {.inline.} =
  cast[Hash](reverseBits(uint64(x) * 15241094284759029579'u64))

import std/sysrand
proc secureSalt*(x: pointer): Hash {.inline.} =
  var buf = urandom(Hash.sizeof.Natural)
  copyMem result.addr, buf[0].addr, Hash.sizeof

proc vmaddrSalt*(x: pointer): Hash {.inline.} =
  const roMuDuoJr = 15241094284759029579'u64  # selected to pair w/27 bit roll
  cast[Hash](rotateLeftBits((cast[uint64](x) shr 3) * roMuDuoJr, 27))

proc zeroSalt*(x: pointer): Hash {.inline.} = 0

when defined(unstableHash):
  var getSalt* = secureSalt
else:
  var getSalt* = vmaddrSalt

when int.sizeof == int64.sizeof:
  proc hashRoMu1*(x: uint): Hash {.inline.} = hashRoMu1(uint64(x))
  proc hashRevFib*(x: uint): Hash {.inline.} = hashRevFib(uint64(x))
else:
  proc hashRoMu1*(x: int|uint): Hash {.inline.} = hashRoMu1(uint32(x))
  proc hashRevFib*(x: int|uint): Hash {.inline.} = hashRevFib(uint32(x))

proc hash*(hsh,salt:Hash):Hash {.inline.}= hashWangYi1(hsh xor cast[Hash](salt))

when defined(hashDebug):
  template dbg*(x) = x
else:
  template dbg*(x) = discard

# These do not really belong here, but `hcodes` needs `Hash` from here anyway.
proc raiseNotFound*[K](key: K) =
  when compiles($key):
    raise newException(KeyError, "key not found: " & $key)
  else:
    raise newException(KeyError, "key not found")

proc normalized*[T](x: openArray[T]): seq[float] =
  var norm = 0.0
  for n in x: norm += n.float
  norm = 1.0 / norm
  result.setLen x.len
  for i, n in x: result[i] = n.float * norm
