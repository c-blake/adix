import strutils, times, metab, althash, cligen, cligen/[mfile, mslice]
proc hash(x: uint64): Hash {.inline.} = hashRoMu1(x) # =~ 1.03x faster

const prime: array[26, uint64] = [ #9/267751 oflow
  7'u64, 61, 41, 53, 2, 71, 47, 29, 3, 97, 89, 17, 59,
  19, 5, 31, 101, 11, 13, 23, 37, 79, 73, 67, 43, 83 ]

proc product(word: MSlice): uint64 =
  result = 1'u64
  for ch in word: result *= prime[ord(ch) - ord('A')]

proc qry(dict="words", stats=false, query: seq[string])=
  let mf = mopen(dict)
  if mf == nil: return
  var ana = initTab[uint64, seq[MSlice]](mf.len div 10)
  let t0 = getTime()
  for word in mf.mSlices:
    ana.mgetOrPut(word.product, @[]).add word
  let t1 = getTime()
  for word in query:
    let word = word.toUpperAscii
    let prod = word.toMSlice.product
    echo word, ":"
    try:
      for ana in ana[prod]: echo "  ", ana
    except: echo "No such word in ", dict
  if stats:
    echo "Build Time: ", (t1 - t0).inMicroseconds, " us"
    when compiles(ana.depths):
      echo "Depths: ", ana.depths    # hash table perf
      echo "FinalTable: ", ana.len, "/", ana.getCap
  mf.close

when isMainModule:
  dispatch(qry, cmdName="anaPrime")
