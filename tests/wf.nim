import std/[heapqueue, hashes, osproc, times],
       adix/lptabz, cligen/[mfile, mslice, osUt], cligen
type
  Word   = distinct uint32
  Count  = uint32
  Histo  = LPTabz[Word, Count, Word, 0]
  ThrDat = tuple[part: ptr MSlice, hp: ptr Histo]

template initHisto(sz): untyped =       # 4*16*8=512B max depth at 65536 entry
  initLPTabz[Word, Count, Word, 0](sz, numer=4, denom=1, robinHood=false)

var mf: MFile
var hs: seq[Histo]                      # NEED -d:useMalloc
var thrs: seq[Thread[ThrDat]]

proc initWord(off, len: int): Word = Word((off.uint32 shl 5) or len.uint32)

proc len(w: Word): uint32 = uint32(w) and 31

proc mem(w: Word): pointer = mf.mem +! int(w.uint32 shr 5)

proc hash(w: Word): Hash {.inline.} = hashData(w.mem, w.len.int)

proc `==`(a, b: Word): bool {.inline.} =
  a.len == b.len and cmemcmp(a.mem, b.mem, a.len) == 0

proc `==`(a: int, b: Word): bool {.inline.} = a == b.int # for z != K(0)

proc `<`(a, b: Word): bool {.inline.} = # for heapqueue
  cmemcmp(a.mem, b.mem, min(a.len, b.len)) < 0

proc `$`(w: Word): string =             # for output
  result.setLen w.len
  copyMem result[0].addr, w.mem, w.len

proc work(td: ThrDat) {.thread.} =
  setAffinity()                         # pin to CPU initially assigned
  var wd, n: int
  for i, ch in td.part[]:
    if ch in {'a'..'z'}:                # in-word ch
      if n == 0: wd = (td.part[].mem +! i) -! mf.mem
      n.inc                             # extend
    elif ch in {'A'..'Z'}:              # `tr A-Z a-z` preprocess to avoid
      td.part[][i] = char(ord(ch) + 32) # needs MAP_PRIVATE
      if n == 0: wd = (td.part[].mem +! i) -! mf.mem
      n.inc
    elif n > 0:                                 # non-word ch
      td.hp[].mgetOrPut(initWord(wd, n), 0).inc # bump word &
      n = 0                                     #..terminate
  if n > 0:                             # part maybe ends in a word
    td.hp[].mgetOrPut(initWord(wd, n), 0).inc

proc count(p: int, path: string) =
  var (mfLoc, parts) = p.nSplit(path, flags=MAP_PRIVATE)
  mf = mfLoc
  if mf != nil:
    if p > 1:                           # add mf.len > 65536|something?
      for i in 0 ..< parts.len:         # spawn workers
        createThread thrs[i], work, (parts[i].addr, hs[i].addr)
      joinThreads thrs
    else:                               # ST-mode does no spawn
      work (parts[0].addr, hs[0].addr)
  else:
    stderr.write "wf: \"", path, "\" missing/irregular\n"

iterator top(h: Histo, n=10, tot: ptr uint32=nil): (Word, Count) =
  var q = initHeapQueue[(Count, Word)]()
  for key, val in h:
    if tot != nil: tot[] += val         # normalizing total, if requested
    let elem = (val, key)               # maintain a heap..
    if q.len < n: q.push(elem)          # ..of the biggest n items
    elif elem > q[0]: discard q.replace(elem)
  var y: (Word, Count)                  # yielded tuple
  while q.len > 0:                      # q now has top n entries
    let r = q.pop
    y[0] = r[1]
    y[1] = r[0]
    yield y                             # yield in ASCENDING order

proc wf(path: seq[string], n=10, grand=false, par=1, sz=9_718, time=false) =
  ## Parallel word frequency tool for one file < 128 MiB and words < 32 chars.
  ## Aggregate multiple via, e.g., `cat **/*.txt > /dev/shm/inp`.
  if path.len != 1: raise newException(ValueError, "only 1 file supported")
  let t0 = epochTime()
  let p = if par > 0: par else: countProcessors()
  thrs.setLen p                         # allocate `thrs` & histos
  for i in 0 ..< p: hs.add initHisto(sz)
  p.count path[0]
  for i in 1 ..< p:                     # hs[0] += [1..<p]
    for wd, cnt in hs[i]: hs[0].mgetOrPut(wd, 0) += cnt
  let n = if n != 0: n else: hs[0].len
  var tot = 0'u32
  for wd, cnt in hs[0].top(n, tot.addr):
    echo cnt, " ", wd                   # print histogram
  if grand: echo tot, " TOTAL"          # with normalizing constant
  if time: stderr.write epochTime() - t0, " sec\n"

dispatch(wf, help={"n": "print top n; 0=>all", "grand": "grand total",
                   "par": "num threads; 0=>auto", "sz": "init size"})
