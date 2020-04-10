# Just writes 8 byte binary hashes of 0..<n to stdout for input to PractRand.
import althash, bitops, cligen, cligen/osUt
type HashFun = enum WangYi, MoreMur, NASAM
proc writeHash(n=99, r=0, fun=WangYi) =
  var h: Hash
  for i in 0 ..< n:
    let x = rotateLeftBits(uint64(i), r)
    case fun
    of WangYi:  h = hashWangYi1(x)
    of MoreMur: h = hashMoreMur(x)
    of NASAM:   h = hashNASAM(x)
    discard stdout.uriteBuffer(cast[cstring](h.addr), Hash.sizeof.Natural)
dispatch(writeHash, help = { "n": "samples", "r": "rotation",
                             "fun": "hash function"})
