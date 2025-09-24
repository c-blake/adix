when not declared(stdin): import std/[syncio, formatfloat]
import std/[hashes,times,strutils], adix/oats, cligen/[mslice,strUt,osUt],cligen

const bLen {.intdefine.} = 16   # <16K long;  RT params better but more work
const bOff {.intdefine.} = 32   # <4G UNIQUE line data
const bCnt {.intdefine.} = 32   # <4 GiCount; Must be 32 for -d=frecent
type
  Counter = (when defined intCnt: uint32 else: float32)
  Count {.packed.} = object     # Dense-ish hash Count type
    when defined hashCache: hc: uint32 # 10B|14B per cell
    len {.bitsize: bLen.}: uint32 # Cmp goes hc, len, key
    off {.bitsize: bOff.}: uint32
    when defined intCnt: cnt {.bitsize: bCnt.}: Counter
    else               : cnt: Counter
  Counts = object
    dat: seq[Count]
    nUsed: int

var s: string; oatKStack s, Counts, Count, off,uint32, MSlice, MSlice
proc key(c: Counts, i: int): MSlice = c.dat[i].key
proc val(c: var Counts, i: int, v: Counter) {.used.} = c.dat[i].cnt = v
proc val(c: Counts, i: int): Counter = c.dat[i].cnt
proc used(c: Counts, i: int): bool = c.dat[i].off != 0
when defined hashCache:         # 2nd def triggers saving lpt behavior
  proc hash(ms: MSlice): Hash = mslice.hash(ms).uint32.Hash
  proc hash(c: var Counts, i: int, hc: Hash) {.used.} = c.dat[i].hc = hc.uint32
  proc hash(c: Counts, i: int): Hash = c.dat[i].hc.Hash
oatCounted c,Counts, c.nUsed; oatSeq Counts, dat  # make counted & resizable
#when Counts is VROat[MSlice, MSlice, Counter]: {.warning: "Counts is a VROat"}

var wMul = 1/0.99       # Set by CLI driver; Used only in "frecent" mode
var wTot = Counter(0)   # Total weight used only for --count & @f normalization
var w    = Counter(1)   # Constant if not frecent, else exponentially growing
let wMax = 3.4028235e33 # 38 = max float32; Leave 1e5 room for `wTot`
proc incFailed(h: var Counts, ms: MSlice): bool =
  var ms = ms
  if ms.len > (1 shl bLen) - 1: # Careful to not overflow XXX rate limit msgs
    erru "truncating too long (", $ms.len, ") line: ", ($ms)[0..<128], "...\n"
  h.upSert(ms, i):              # Found key @i:
    if (when defined intCnt: h.dat[i].cnt == (1 shl bCnt) - 1 else: false):
      erru "counter overflow for: ",$ms,"\n" # no update XXX rate limit msgs
    else: h.dat[i].cnt += w; wTot += w  # bump
  do:                           # Novel key->i:
    h.dat[i].off = s.add(ms, (1 shl bOff) - 1):
      erru "unique line data overflow at:",$ms,"\n" #XXX rate limit msgs
      return true               # Cannot go on GLOBALLY
    h.dat[i].len = ms.len.uint32 # Init
    h.dat[i].cnt = w; wTot += w
  when not defined(intCnt):     # Do 1-param frecency idea; Simpler than firefox
    w *= wMul                   # Always-grow-for-new-data EWMA update
    if w > wMax:                # Nearing per-key weight repr limit..
      let sclInv = 1.0/wMax     # *= twice since this pushes FP repr limits
      for c in mitems h.dat: c.cnt *= sclInv; c.cnt *= sclInv
      w *= sclInv; w *= sclInv        # These *='s move all FP numbers from..
      wTot *= sclInv; wTot *= sclInv  #..near top of repr to near bottom.

proc lfreq(n=10, count=false, size=9999, dSize=81920, recTerm='\n',
           format="@c @k", RecTerm="\n", old=1.0, tm=false) =
  ## Histogram `stdin` lines (read w/non-memory mapped IO to be pipe friendly).
  ## Limits: <4 GiB unique data; <16 KiB lines; <4 GiCount.  If `old < 1.0`,
  ## frequency -> simple 1-parameter "frecency" where counts are decayed by a
  ## factor `old` (virtually) after each line (i.e. by index not wall time).
  let t0 = if tm: epochTime() else: 0.0
  var h: Counts; h.setCap size  # pre-size table & data
  s.setLen dSize; s.setLen 1    # `1` here lets us encode empty as 0-offset
  when not defined(intCnt): wMul = 1.0/old
  block IO:
    for (line, nLine) in stdin.getDelims(recTerm):
      let ms = MSlice(mem: line, len: nLine - 1)
      if h.incFailed(ms): break IO
  if count: outu h.len," unique ",wTot," total ",s.len," B\n"
  let wInv = 1.0/wTot.float; var cs, fs: string # Setup for..
  let prs = format.tmplParsed('@')              #..nice output
  template output =
    for (id, arg, call) in prs:
      if id.idIsLiteral: outu MSlice(mem: format[arg.a].addr, len: arg.len)
      elif format[id.a] == 'k': outu k
      elif format[id.a] == 'c':
        when defined intCnt: cs.setLen 0; cs.addInt c; outu cs
        else               : cs.setLen 0; cs.ecvt c.float, 6; outu cs
      elif format[id.a] == 'f': fs.setLen 0; fs.fcvt c.float*wInv, 9; outu fs
      else: outu MSlice(mem: format[call.a].addr, len: call.len)
    outu RecTerm
  if   n == 0: (for (k, c) in pairs(h): output())
  elif n > 0 : (for (k, c) in topByVal[MSlice,MSlice,Counter](h, n): output())
  elif n < -1: (for (k, c) in topByVal[MSlice,MSlice,Counter](h, -n, order=Descending): output())
  if tm: stderr.write epochTime() - t0, "\n"  # -n-1 for only time output

when isMainModule: dispatch lfreq, help={
  "n"    : "emit `n`-most common  lines(0:all; <0 sorted)",
  "count": "only emit counts: unique & grand total",
  "size" : "pre-size hash table for size unique entries",
  "dSize": "pre-size str data area to this many bytes",
  "recTerm": "input record terminator",
  "RecTerm": "output record terminator",
  "format" : "output format: @k=key @c=count @f=fraction",
  "old"  : "exponen.weight for 'old' ages (if not intCnt)",
  "tm"   : "emit wall time of counting to stderr & quit"}
