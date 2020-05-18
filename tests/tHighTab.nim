import metab, althash, times

const shift = 49'u
proc hash(x: uint64): Hash = hashRoMu1(x)

var one = initTab[uint64, int](4, rehash=false)
var t0 = epochTime()
for i in 0'u ..< ((1'u shl 15) - 1):
  one[uint64(i shl shift)] = 2
echo epochTime() - t0, " seconds"
var ds = one.depths
echo ds
echo "MAX DEPTH: ", ds.len
echo one.len, "/", one.getCap
echo "Stats: ", one.depthStats
# one.debugDump
