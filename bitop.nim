## This is a reimplementation of some things we need from bitops which has CT
## trouble due to importc's.  (I feel it's a better naming/factoring, too).

proc ceilPow2*(x: int): int {.noSideEffect, inline.} =
  ## Returns ``x`` rounded up to the nearest power of two.
  ## Zero and negative numbers get rounded up to 1.
  runnableExamples:
    doAssert ceilPow2(16) == 16
    doAssert ceilPow2(5) == 8
    doAssert ceilPow2(0) == 1
    doAssert ceilPow2(-16) == 1
  result = x - 1
  when defined(cpu64):
    result = result or (result shr 32)
  when sizeof(int) > 2:
    result = result or (result shr 16)
  result = result or (result shr 8)
  result = result or (result shr 4)
  result = result or (result shr 2)
  result = result or (result shr 1)
  result += 1 + ord(x <= 0)

proc floorPow2*(x: int): int {.noSideEffect, inline.} =
  ## Returns ``x`` rounded down to the nearest power of two.
  result = result or (result shr 1)
  result = result or (result shr 2)
  result = result or (result shr 4)
  result = result or (result shr 8)
  when sizeof(int) > 2:
    result = result or (result shr 16)
  when defined(cpu64):
    result = result or (result shr 32)
  result = result - (result shr 1)

# https://stackoverflow.com/questions/3465098/bit-twiddling-which-bit-is-set/
# This is essentially just a perfect hash a la Leiserson98-UsingDeBruijnSeqs.
when defined(cpu64):
  const deBruijn8 = [ 0, 1, 2, 53, 3, 7, 54, 27, 4, 38, 41, 8, 34, 55, 48, 28,
    62, 5, 39, 46, 44, 42, 22, 9, 24, 35, 59, 56, 49, 18, 29, 11, 63, 52, 6,
    26, 37, 40, 33, 47, 61, 45, 43, 21, 23, 58, 17, 10, 51, 25, 36, 32, 60, 20,
    57, 16, 50, 31, 19, 15, 30, 14, 13, 12 ]
else:
  const deBruijn4 = [ 0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17,
    4, 8, 31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9 ]

proc lgPow2*(x: int): int {.inline.} =
  when defined(cpu64):
    deBruijn8[(uint64(x) * 0x022FDD63CC95386D'u64) shr 58]
  else:
    deBruijn4[(uint32(x) * 0x077CB531'u32) shr 27]

proc lgCeil*(x: int): int {.inline.} =
  ## integer-math only impl of ceil(log2(x))
  lgPow2(ceilPow2(x))

proc lgFloor*(x: int): int {.inline.} =
  ## integer-math only impl of floor(log2(x))
  lgPow2(floorPow2(x))

proc lg*(x: int): int {.inline.} =
  ## short alias for lgCeil
  lgCeil(x)

proc rotateLeftBits*(a: uint64, numBits: int): uint64 {.inline.} =
  ## like bitops
  result = (a shl numBits) or (a shr (uint64.sizeof * 8 - numBits))

proc rotateRightBits*(a: uint, numBits: int): uint {.inline.} =    # like bitops
  ## like bitops
  result = (a shr numBits) or (a shl (uint.sizeof * 8 - numBits))

proc reverseBitsByte*(x: uint8): uint8 {.inline.} =
  const reversed = [ 0b0000'u8, 0b1000, 0b0100, 0b1100,
                     0b0010   , 0b1010, 0b0110, 0b1110,
                     0b0001   , 0b1001, 0b0101, 0b1101,
                     0b0011   , 0b1011, 0b0111, 0b1111 ]
  result = (reversed[x and 15] shl 4) or reversed[x shr 4]

proc reverseBitsMakeTable(): array[256, uint8] =
  for i in 0 ..< 256:
    result[i] = reverseBitsByte(uint8(i))

const revByte = reverseBitsMakeTable()

proc reverseBits*(x: uint32): uint32 =
  result = (uint32(revByte[int((x and 0x000000FF'u32)       )]) shl 24) or
           (uint32(revByte[int((x and 0x0000FF00'u32) shr  8)]) shl 16) or
           (uint32(revByte[int((x and 0x00FF0000'u32) shr 16)]) shl  8) or
            uint32(revByte[int( x                             shr 24)])

proc reverseBits*(x: uint64): uint64 =
  result = (uint64(revByte[int((x and 0x00000000000000FF'u64)      )]) shl 56)or
           (uint64(revByte[int((x and 0x000000000000FF00'u64)shr  8)]) shl 48)or
           (uint64(revByte[int((x and 0x0000000000FF0000'u64)shr 16)]) shl 40)or
           (uint64(revByte[int((x and 0x00000000FF000000'u64)shr 24)]) shl 32)or
           (uint64(revByte[int((x and 0x000000FF00000000'u64)shr 32)]) shl 24)or
           (uint64(revByte[int((x and 0x0000FF0000000000'u64)shr 40)]) shl 16)or
           (uint64(revByte[int((x and 0x00FF000000000000'u64)shr 48)]) shl  8)or
            uint64(revByte[int( x                            shr 56)])

proc isPow2*(x: int): bool =
  if x == 0: return false
  (x and (x - 1)) == 0
