when not declared(addFloat): import std/[formatfloat, objectdollar]
import metab, althash, times#, math
#                                 0  1  2  3  4  5    6    7  8   9
#tombstone methods get depths ~ @[2, 1, 1, 1, 1, 32, 174, 40, 2, 1]
#That is "roughly" a counter example for Python probe sequence, but it would be
#better to push a lot more past depth 7 since 7x slower maybe isn't so obvious.
#
#rehash/robinhood mitigations are not enough in `lpset`.  What seems to work
#well is defining below hash which then makes actual hash RoMu1(RoMu1(x)).
#
#proc hash(x: uint64): Hash = hashRoMu1(x)
#proc hash(x: uint64): Hash = hashRoMu2(x) # good hash for these keys @any shift

const shift = 56'u

var one = initSet[uint64](256, robinhood=true, rehash=true, numer=10, denom=1)
var t0 = epochTime()
for i in 0'u ..< 234:
  one.incl uint64(i shl shift)
echo (epochTime() - t0)*1e9/234.0, " ns/elt"
echo one.len, "/", one.getCap, " = ", one.len.float/one.getCap.float
echo "Depths:",  one.depths
echo "Stats: ", one.depthStats
one.debugDump
