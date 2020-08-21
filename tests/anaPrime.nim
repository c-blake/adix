import strutils, times, iltab, althash, cligen, cligen/[mfile, mslice]
proc hash(x: uint64): Hash {.inline.} = hashRoMu1(x) # =~ 1.03x faster

proc sig(word: MSlice): uint64 {.inline.} = # word signature
  const prime = [ #9/267751 oflow
    7'u64, 61, 41, 53, 2, 71, 47, 29, 3, 97, 89, 17, 59,
    19, 5, 31, 101, 11, 13, 23, 37, 79, 73, 67, 43, 83 ]
  result = 1'u64
  for ch in word: result *= prime[ord(ch) - ord('A')]

proc qry(dict="words", stats=false, query: seq[string]) =
  let t0 = getTime()
  let mf = mopen(dict)
  if mf == nil: return
  # tables should allow file-backed allocation to make saving answer easy
  var ana = initILTab[uint64, uint32, 0](mf.len div 10, numer=3, denom=1)
  var wds = newSeqOfCap[MSlice](mf.len div 10)
  for word in mf.mSlices:
    ana.add word.sig, uint32(wds.len)
    wds.add word
  let t1 = getTime()
  for word in query:
    let word = word.toUpperAscii
    let prod = word.toMSlice.sig
    echo word, ":"
    for ana in ana.allValues(prod):
      echo "  ", wds[ana]
  if stats:
    echo "Build Time: ", (t1 - t0).inMicroseconds, " us"
    when compiles(ana.depths):
      echo "Depths: ", ana.depths    # hash table perf
      echo "FinalTable: ", ana.len, "/", ana.getCap
  mf.close

when isMainModule: dispatch(qry, cmdName="anaPrime")
