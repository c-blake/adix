when not declared(stdin): import std/[syncio, formatfloat]
import adix/lptabz {.all.}, cligen, cligen/[mslice, osUt], std/times
var str: string                 # Big (NON-RELOCATABLE!) stack of string data

proc lfreq(n=0, count=false,uniq=false,Norm=false, size=7, dSize=99, tm=false)=
  ## Histogram `stdin` lines (read w/non-memory mapped IO to be pipe friendly).
  ## (Needs manual dSize tuning of non-movable string stack.)
  let t0   = epochTime()
  var nTot = 0
  str.setLen dSize; str.setLen 0
  var cnt  = initLPTab[MSlice, int](size) # HCell 16+8+8=32B
  for (line, nLine) in stdin.getDelims:
    let ms = MSlice(mem: line, len: nLine - 1)
    inc nTot                    # Always bump `nTotal`
    cnt.getPut(i, ms) do:       # Found key @i:
      cnt.cell(i).val.inc       #   bump
    do:                         # Novel key->i:
      let off = str.len         #   alloc, copy, init
      if off + ms.len+1 > dSize:raise newException(ValueError,"dSize too small")
      str.setLen off + ms.len+1 #   a noInit would be nice
      copyMem str[off].addr, ms.mem, ms.len
      cnt.cell(i).key = MSlice(mem: str[off].addr, len: ms.len)
      cnt.cell(i).val = 1
  if count:
    echo cnt.len," unique ",nTot," total"
  template o =
    if not uniq:
      if Norm: stdout.urite c.float / nTot.float
      else: stdout.urite c
      stdout.urite " "
    stdout.urite k; stdout.urite "\n"
  if  n == 0: (for (k, c) in pairs(cnt): o())       # unsorted
  elif n > 0: (for (k, c) in cnt.topByVal(n): o())  # top n
  if tm: stderr.write epochTime() - t0, " sec\n"    # n<0 => only `c`/tm

when isMainModule: dispatch lfreq, help={
  "n"    : "only emit most frequent `n` lines (!=0=>sorted)",
  "count": "only emit counts: unique & grand total",
  "uniq" : "only emit unique lines, not frequencies",
  "Norm" : "normalize frequencies by dividing by grand tot",
  "size" : "pre-size hash table for size unique entries",
  "dSize": "size string data area to this many bytes",
  "tm"   : "emit elapsed wall time to stderr"}
