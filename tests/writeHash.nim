# Just writes 8 byte binary hashes of 0..<n to stdout for input to PractRand.
import althash, bitops, times, cligen, cligen/osUt

type HashFun = enum WangYi, MoreMur, NASAM

proc writeHash(n=99, r=0, fun=WangYi, bench=false) =
  var h, sum: Hash
  let t0 = getTime()
  for i in 0 ..< n:
    let x = rotateLeftBits(uint64(i), r)
    case fun
    of WangYi:  h = hashWangYi1(x)
    of MoreMur: h = hashMoreMur(x)
    of NASAM:   h = hashNASAM(x)
    if bench:
      sum += h
    else:
      discard stdout.uriteBuffer(cast[cstring](h.addr), Hash.sizeof.Natural)
  if bench:
    echo "sum: ", sum, " ns/hash: ", (getTime() - t0).nanoseconds.float / n.float

dispatch(writeHash, help = { "n": "samples", "r": "rotation",
                             "fun": "hash function"})

# For 1e6 numbers on 3 computers.  Time is min(5 runs) in ns/hash.
# Compiled with Nim ...d0942d42a1; gcc-10.0.1 with PGO.
#   CPU/Hash  WangYi  MoreMur  NASAM
#   i76700k   1.2843  1.4986   1.6379  (4.70 GHz)
#   amd2950x  1.7293  1.9127   2.2522  (3.78 GHz)
#   i5-540m   2.9324  3.1191  11.9170  (2.53 GHz)