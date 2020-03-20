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
## likley more natural than the low order avalanche needed for ``and mask``.
## Both may count as what https://burtleburtle.net/bob/hash/integer.html calls
## "half avalanche".  Anyway, rotating is portably fast (often 1 cycle latency,
## 1-per cycle tput for an immediate constant number of bits).  Bit reversal is
## not so slow using a fully unrolled loop and 16 entry nibble lookup table.
## Different hashes will perform more/less well on different data.  So, we just
## provide a few here, and one is based upon highly non-linear bit reversal.
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

proc hashRoMu1*(x: int64|uint64): Hash {.inline.} =
  ## 1-hop of Romul-DuoJr using x as seed
  Hash(rotateLeftBits(x * 15241094284759029579'u64, 27))

proc hashRevFib*(x: int32|uint32): Hash {.inline.} =
  Hash(reverseBits(uint32(x) * 0xd3833e81'u64))

proc hashRevFib*(x: int64|uint64): Hash {.inline.} =
  Hash(reverseBits(uint64(x) * 15241094284759029579'u64))

# inspired from https://gist.github.com/badboy/6267743
#	var x = x xor ((x shr 20) xor (x shr 12))
#	result = cast[Hash](x xor (x shr 7) xor (x shr 4))
# Thomas Wang, Jan 1997

proc secureSalt*(x: pointer): Hash {.inline.} =
  proc getrandom(buf: pointer, len: csize_t, flags: cuint): csize {. importc:
    "getrandom", header: "sys/random.h" .}
  stderr.write "secure salt\n"
  discard getrandom(result.addr, csize_t(result.sizeof), cuint(0))

proc vmaddrSalt*(x: pointer): Hash {.inline.} =
  const roMuDuoJr = 15241094284759029579'u64  # selected to pair w/27 bit roll
  Hash(rotateLeftBits((cast[uint64](x) shr 3) * roMuDuoJr, 27))

proc zeroSalt*(x: pointer): Hash {.inline.} = 0

var getSalt* = zeroSalt

when int.sizeof == int64.sizeof:
  proc hashRoMu1*(x: int|uint): Hash {.inline.} = hashRoMu1(uint64(x))
  proc hashRevFib*(x: int|uint): Hash {.inline.} = hashRevFib(uint64(x))
else:
  proc hashRoMu1*(x: int|uint): Hash {.inline.} = hashRoMu1(uint32(x))
  proc hashRevFib*(x: int|uint): Hash {.inline.} = hashRevFib(uint32(x))

when defined(unstableHashHash):
  proc hash*(hashValue, salt: Hash): Hash {.inline.} =
    hashRoMu1(hashValue) xor Hash(salt)
else:
  proc hash*(hashValue, salt: Hash): Hash {.inline.} =
    hashRoMu1(hashValue)

when defined(hashDebug):
  template dbg*(x) = x
else:
  template dbg*(x) = discard
