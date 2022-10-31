when not declared(addFloat): import std/formatfloat
import metab, althash, times

let iniSz = 4
#let iniSz = 1 shl 15
#let iniSz = 1 shl 18
#let iniSz = 1 shl 23
when defined(startWithRehash):
  let rh = true
else:
  let rh = false
when defined(althash):
  proc hash(x: int): Hash = hashRoMu1(x)
elif defined(althash2):
  proc hash(x: int): Hash = hashRevFib(x)

echo "START... "
var t0 = epochTime()

const shift = 25

var one = initSet[int](iniSz, rehash=rh)
#for i in 0 ..< ((1 shl 15) - 1): one.incl i
for i in 0 ..< ((1 shl 23) - 19): one.incl (i shl shift)
echo epochTime() - t0, " seconds"
var ds = one.depths
echo ds
echo "MAX DEPTH: ", ds.len
echo one.len, "/", one.getCap
echo "Stats: ", one.depthStats
# one.debugDump

echo "LOOKING UP... "
for i in 0 ..< ((1 shl 23) - 19):
  if (i shl shift) notin one: echo i, " WAS MISSING"
echo "CLONING... "

t0 = epochTime()
var two = initSet[int](iniSz, rehash=rh)
for v in one: two.incl v
echo epochTime() - t0, " seconds"
ds = two.depths
echo ds
echo "MAX DEPTH: ", ds.len
echo two.len, "/", two.getCap
echo "Stats: ", two.depthStats
# two.debugDump

echo "DONE... "
