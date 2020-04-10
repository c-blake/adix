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
