when not declared(stdin): import std/[syncio, formatfloat]
import adix/lptabz {.all.}, cligen, cligen/[mslice, osUt]

proc lfreq(size=1000, n=0, count=false, grand=false, Norm=false) =
  ## Histogram `stdin` lines (read w/non-memory mapped IO to be pipe friendly).
  var cnt  = initLPTab[MSlice, int](size) # HCell 16+8+8=32B
  var nTot = 0
  var str  = ""                 # A big stack of string data
  for (line, nLine) in stdin.getDelims:
    let ms = MSlice(mem: line, len: nLine - 1)
    inc nTot                    # Always bump `nTotal`
    cnt.getPut(i, ms) do:       # Found key @i:
      cnt.cell(i).val.inc       #   bump
    do:                         # Novel key->i:
      let off = str.len         #   alloc, copy, init
      str.setLen off + ms.len   #   a noInit would be nice
      copyMem str[off].addr, ms.mem, ms.len
      cnt.cell(i).key = MSlice(mem: str[off].addr, len: ms.len)
      cnt.cell(i).val = 1
  if count:
    echo cnt.len," uniqueLines ",nTot," totalLines"; return
  if grand:                     # Write early so invoker can..
    stderr.write nTot, '\n'     #..normalize with 1 less pass.
  template output =
    if Norm: stdout.urite c.float / nTot.float
    else: stdout.urite c
    stdout.urite " "; stdout.urite k; stdout.urite "\n"
  if n == 0: (for (k, c) in pairs(cnt): output())
  else: (for (k, c) in cnt.topByVal(n): output())

when isMainModule: dispatch lfreq, help={
  "size" : "pre-size hash table for size unique entries",
  "n"    : "only emit most frequent `n` lines (!=0=>sorted)",
  "count": "only emit counts: unique & grand total",
  "grand": "total line count -> stderr (BEFORE main output)",
  "Norm" : "normalize frequencies by dividing by grand tot"}

#NOTE: 16B=Eg hc:30,off:34,len:30,cnt:34 has reasonable limits & if 1/2 very rnd
# access memory saves you from a mem.hierarchy cliff that can matter by 2..1000X
# BUT also requires a more user-defined-exported-to-lptabz like HCell interface.
# https://github.com/c-blake/bst is an e.g. of an ANSI C89 interface for such.
