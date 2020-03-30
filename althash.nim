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
## called "middle square" based on this pattern).
##
## The full product is easily accessed for half CPU register width and narrower
## numbers.  For the widest integer type, C/backend arithmetic rules only give
## the modulus|lower order bits of said product (x86/amd64 does make the upper
## word available in another register).  We take the simple portable way out and
## discard half our key entropy, just rotating down 32 bits, though bit reversal
## might be better (though expensive) and ``shr`` reduction from hcode to table
## address is another way out.  In general achieving "high order avalanche" is
## likely more natural than the low order avalanche needed for ``and mask``.
## Both may count as what https://burtleburtle.net/bob/hash/integer.html calls
## "half avalanche".  Anyway, rotating is portably fast (often 1 cycle latency,
## 1-per cycle tput for an immediate constant number of bits).  Bit reversal is
## not so slow using a fully unrolled loop and 16 entry nibble lookup table.
## Different hashes will perform more/less well on different data.  So, we just
## provide a few here, and one is based upon highly non-linear bit reversal.
##
## The strongest hash is hashWY which passes all of SMHasher's entropy tests
## and so is the default Hash-rehasher.
##
## (Incidentally, most "fast in GB/s" hashes are far too slow for just one int.
## Even assessing them that way for lookup tables is misleading.  You want time
## =~ a + b*nBytes (at least) where a & b maybe come from a linear regression.
## Just 1/b tells you little, especially for integer keys where ``a`` dominates,
## although short string hashes can be similarly misleading.)

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

#XXX should do 1B, 2B, 4B versions as well.
proc HiXorLo(A, B: uint64): uint64 {.inline.} =
  # Xor of high & low 8B of full 16B product
  when defined(gcc) or defined(llvm_gcc) or defined(clang):
    {.emit: """__uint128_t r = A; r *= B; return (r >> 64) ^ r; """.}
  elif defined(windows) and not defined(tcc):
    {.emit: """A = _umul128(A, B, &B); return A ^ B;""".}
  else: # Fall back for weaker platforms/compilers, e.g. Nim VM, etc.
    let
      ha = A shr 32
      hb = B shr 32
      la = A and 0xFFFFFFFF'u64
      lb = B and 0xFFFFFFFF'u64
      rh  = ha * hb
      rm0 = ha * lb
      rm1 = hb * la
      rl  = la * lb
      t   = rl + (rm0 shl 32)
    var c = if t < rl: 1'u64 else: 0'u64
    let lo = t + (rm1 shl 32)
    c += (if lo < t: 1'u64 else: 0'u64)
    let hi = rh + (rm0 shr 32) + (rm1 shr 32) + c
    return hi xor lo

proc hashWY*(x: int64|uint64|Hash): Hash {.inline.} =
  ## Wang Yi's hash for 8B int.  NOTE: It is fine to define
  ## ``proc(x: int16): Hash {.inline.} = hashWY(Hash(x))``.
  ## See https://github.com/rurban/smhasher for more details.
  const P0 = 0xa0761d6478bd642f'u64
  const P1 = 0xe7037ed1a0b428db'u64
  const P5x8 = 0xeb44accab455d165'u64 xor 8'u64
  Hash(HiXorLo(HiXorLo(P0, uint64(x) xor P1), P5x8))

proc hashWY0*(x: int64|uint64|Hash): Hash {.inline.} =
  ## A slightly simplified/early version of Wang Yi's hash for 8B ints.
  ## Faster, but less scrambling.  Definitely has some easy weak spots.
  const P0 = 0xa0761d6478bd642f'u64
  const P1 = 0xe7037ed1a0b428db'u64
  let x = uint64(x)
  Hash(HiXorLo(P0, x xor P1))

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
  proc hash*(hsh, salt: Hash): Hash {.inline.} = hashWY(hsh) xor Hash(salt)
else:
  proc hash*(hsh, salt: Hash): Hash {.inline.} = hashWY(hsh)

when defined(hashDebug):
  template dbg*(x) = x
else:
  template dbg*(x) = discard
