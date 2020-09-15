import strutils, times, lptabz, althash, cligen, cligen/[mfile, mslice]
proc hash(x: uint64): Hash {.inline.} = hashRoMu1(x) # =~ 1.05x faster

type Word = distinct uint32 # 24 bits of byte-offset, 8 bits of word length

proc initWord(off, len: int): Word {.inline.} =
  Word(uint32(off) shl 8 or uint32(len))

proc toString(w: Word, mf: MFile): string {.inline.} =
  let off = uint32(w) shr 8
  let len = uint32(w) and 255
  result.setLen len
  copyMem result[0].addr, mf.mem +! off, len

proc sig(word: MSlice): uint64 {.inline.} = # word signature
  const prime = [ #9/267751 oflow
    7'u64, 61, 41, 53, 2, 71, 47, 29, 3, 97, 89, 17, 59,
    19, 5, 31, 101, 11, 13, 23, 37, 79, 73, 67, 43, 83 ]
  result = 1'u64
  for ch in word: result *= prime[ord(ch) - ord('A')]

proc getAna(dict="words", mf: MFile): LPTabz[uint64,Word,uint64,0] =
  try: result.mmap(findPathPattern(dict & '.'))
  except:
    result.init(mf.len div 10, numer=3, denom=1)
    for word in mf.mSlices:
      result.add word.sig, initWord(word.mem -! mf.mem, word.len)
    result.save(dict)

proc qry(dict="words", stats=false, query: seq[string]) =
  let t0 = getTime()
  if (let mf = mopen(dict); mf) != nil:
    let ana = dict.getAna(mf)
    let t1 = getTime()
    for word in query:
      let word = word.toUpperAscii
      let key = word.toMSlice.sig
      echo word, ":"
      for ana in ana.allValues(key):
        echo "  ", ana.toString(mf)
    if stats:
      echo "Prep Time: ", (t1 - t0).inMicroseconds, " us"
      when compiles(ana.depths):
        echo "Depths: ", ana.depths    # hash table perf
        echo "FinalTable: ", ana.len, "/", ana.getCap
    mf.close

when isMainModule: dispatch(qry, cmdName="anaPrime")
