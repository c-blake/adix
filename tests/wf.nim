when not declared(addFloat): import std/[formatfloat, typedthreads]
when not declared(Thread): import std/threads
import std/[hashes,osproc,times], adix/lptabz, cligen/[mfile,mslice,osUt],cligen
type
  Word   = distinct uint32
  Count  = uint32
  Histo  = LPTabz[Word, Count, Word, 0]
  ThrDat = tuple[part: ptr MSlice, hp: ptr Histo, nT: ptr uint64]

template initHisto(sz): untyped =       # 4*16*8=512B max depth at 65536 entry
  initLPTabz[Word, Count, Word, 0](sz, numer=4, denom=1, robinHood=false)

var mf: MFile
var hs: seq[Histo]                      # NEED -d:useMalloc
var nTs: seq[uint64]
var thrs: seq[Thread[ThrDat]]

const wb = 5                            # word len bits
const wm = (1 shl wb) - 1               # max word len

proc initWord(off, len: int): Word =
  if len > wm:
    var s = newStringOfCap(len)
    copyMem s[0].addr, mf.mem +! off, len
    raise newException(RangeDefect, "\"" & s & "\" too long")
  Word((off.uint32 shl wb) or len.uint32)

proc len(w: Word): uint32 = uint32(w) and wm

proc mem(w: Word): pointer = mf.mem +! int(w.uint32 shr wb)

# Case insens hash/==|Local stack allocator | may be faster than MAP_PRIVATE.
proc hash(w: Word): Hash {.inline.} =
  hash toOpenArray[byte](cast[ptr UncheckedArray[byte]](w.mem), 0, w.len.int-1)

proc `==`(a, b: Word): bool {.inline.} =
  a.len == b.len and cmemcmp(a.mem, b.mem, a.len) == 0

proc `<`(a, b: Word): bool {.inline.} = # for topk.push
  let c = cmemcmp(a.mem, b.mem, min(a.len, b.len))
  if c == 0: a.len < b.len else: c < 0

proc `$`(w: Word): string =             # for output
  result = newString(w.len)
  copyMem result.cstring, w.mem, w.len

when defined(benhoyt): # Ben Hoyt definition of "words"
 iterator lowCaseWords(ms: MSlice): Word =
  var wd, n: int = 0
  for i, ch in ms:
    if ch in {'A'..'Z'}:                # `tr A-Z a-z` preprocess to avoid
      ms[i] = char(ord(ch) + 32)        # needs MAP_PRIVATE
      if n == 0: wd = (ms.mem +! i) -! mf.mem
      n.inc                             # extend
    elif ord(ch) > ord(' '):            # in-word ch
      if n == 0: wd = (ms.mem +! i) -! mf.mem
      n.inc                             # extend
    elif n > 0:                         # non-word ch & have data
      yield initWord(wd, n); n = 0      # yield & reset
  if n > 0: yield initWord(wd, n)       # any final word
else: # Knuth-McIlroy definition of "words"
 iterator lowCaseWords(ms: MSlice): Word =
  var wd, n: int = 0
  for i, ch in ms:
    if ch in {'a'..'z'}:                # in-word ch
      if n == 0: wd = (ms.mem +! i) -! mf.mem
      n.inc                             # extend
    elif ch in {'A'..'Z'}:              # `tr A-Z a-z` preprocess to avoid
      ms[i] = char(ord(ch) + 32)        # needs MAP_PRIVATE
      if n == 0: wd = (ms.mem +! i) -! mf.mem
      n.inc                             # extend
    elif n > 0:                         # non-word ch & have data
      yield initWord(wd, n); n = 0      # yield & reset
  if n > 0: yield initWord(wd, n)       # any final word

proc work(td: ThrDat) {.thread.} =      # Histogram one segment of an mmap
  setAffinity()                         # pin to CPU initially assigned
  var nT = 0u64                         # Local accumulator to not thrash
  for w in td.part[].lowCaseWords:
    nT.inc; td.hp[].mgetOrPut(w, 0).inc
  td.nT[] = nT

proc count(p: int, path: string) =      # split path into `p` ~equal segments
  var (mfLoc, parts) = p.nSplit(path, flags=MAP_PRIVATE)
  mf = mfLoc
  if mf != nil:
    if mf.len > 1 shl (32 - wb):
      raise newException(RangeDefect, "\"" & path & "\" too large")
    if p > 1:                           # add mf.len > 65536|something?
      for i in 0 ..< parts.len:         # spawn workers
        createThread thrs[i], work, (parts[i].addr, hs[i].addr, nTs[i].addr)
      joinThreads thrs
    else: work (parts[0].addr, hs[0].addr, nTs[0].addr) # ST-mode: No spawn
  else: stderr.write "wf: \"", path, "\" missing/irregular\n"

proc wf(path:seq[string], n=10, c=false, N=false, jobs=1, sz=9999, tm=false) =
  ## Parallel word frequency tool for one file < 128 MiB and words < 32 chars.
  ## Aggregate multiple via, e.g., `cat \*\*/\*.txt > /dev/shm/inp`.  Similar
  ## to Knuth-McIlroy `tr A-Z a-z|tr -sc a-z \\n|sort|uniq -c|sort -n|tail`,
  ## but ~46X faster than the pipeline (on TOTC; depends on vocab).
  let path = if path.len > 1: path[1] else: "/dev/stdin"
  let t0 = epochTime()
  let p = if jobs > 0: jobs else: countProcessors()
  thrs.setLen p                         # allocate `thrs` & histos
  for i in 0 ..< p: hs.add initHisto(sz); nTs.add 0u64
  p.count path
  for i in 1 ..< p:                     # hs[0] += [1..<p]
    nTs[0] += nTs[i]
    for wd, cnt in hs[i]: hs[0].mgetOrPut(wd, 0) += cnt
  if c: echo hs[0].len," unique ",nTs[0]," total"
  template o = echo (if N: $(c.float/nTs[0].float) else: $c)," ",w
  if   n == 0: (for w, c in hs[0].pairs: o())       # unsorted whole
  elif n > 0 : (for w, c in hs[0].topByVal(n): o()) # unsorted top N
  elif n < -1: (for w, c in hs[0].topByVal(n, order=Descending): o()) # sorted
  if tm: stderr.write epochTime() - t0, " sec\n"    # n == -1: only `c`/tm

dispatch(wf, help={"n": "do top n; 0all,<0sort", "c": "count only", "N": "norm",
  "tm": "time", "jobs": "num threads; 0=>auto", "sz": "init size"})
