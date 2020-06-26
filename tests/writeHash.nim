# Just writes 8 byte binary hashes of 0..<n to stdout for input to PractRand.
import althash, bitops, times, cligen, cligen/osUt, strutils

type HashFun =
  enum Identity, RoMu, WangYi, MoreMur, NASAM, SplitMix, Split64, Degski

proc writeHash(n=99, r=0, fun=WangYi, bench=false, step=1, Hex=false) =
  var h, sum: Hash
  let t0 = getTime()
  for j in 0 ..< n:
    let i = j * step
    let x = rotateLeftBits(uint64(i), r)
    case fun
    of Identity: h = hashIdentity(x)
    of RoMu:     h = hashRoMu1(x)
    of WangYi:   h = hashWangYi1(x)
    of MoreMur:  h = hashMoreMur(x)
    of NASAM:    h = hashNASAM(x)
    of SplitMix: h = hashSplitMix(x)
    of Split64:  h = hashSplit64(x)
    of Degski:   h = hashDegski(x)
    if bench:
      sum += h
    else:
      if Hex:
        let s = BiggestInt(h).toHex(16) & '\n'
        discard stdout.uriteBuffer(cstring(s), 17)
      else:
        discard stdout.uriteBuffer(cast[cstring](h.addr), Hash.sizeof.Natural)
  if bench:
    echo "sum: ", sum, " ns/hash: ", (getTime()-t0).inNanoseconds.float/n.float

dispatch(writeHash, help = { "n": "samples", "r": "rotation",
                             "fun": "hash function"})

# For 1e6 numbers on 3 computers.  Time is min(5 runs) in ns/hash.
# Compiled with Nim ...d0942d42a1; gcc-10.0.1 with PGO.
#   CPU/Hash  WangYi  MoreMur  NASAM
#   i76700k   1.2843  1.4986   1.6379  (4.70 GHz)
#   amd2950x  1.7293  1.9127   2.2522  (3.78 GHz)
#   i5-540m   2.9324  3.1191  11.9170  (2.53 GHz)
