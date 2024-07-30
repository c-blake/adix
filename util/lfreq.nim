when not declared(stdin): import std/[syncio, formatfloat]
import std/[hashes,times,strutils], adix/oats, cligen/[mslice,strUt,osUt],cligen

const bLen {.intdefine.} = 16   # <16K long;  RT params better but more work
const bOff {.intdefine.} = 32   # <4G UNIQUE line data
const bCnt {.intdefine.} = 32   # <4 GiCount
type
  Count {.packed.} = object     # Dense-ish hash Count type
    when defined hashCache: hc: uint32 # 10B|14B per cell
    len {.bitsize: bLen.}: uint32 # Cmp goes hc, len, key
    off {.bitsize: bOff.}: uint32
    cnt {.bitsize: bCnt.}: uint32
  Counts = object
    dat: seq[Count]
    nUsed: int

var s: string; oatKStack s, Counts, Count, off,uint32, MSlice, MSlice
proc key(c: Counts, i: int): MSlice = c.dat[i].key
proc val(c: var Counts, i: int, v: uint32) {.used.} = c.dat[i].cnt = v
proc val(c: Counts, i: int): uint32 = c.dat[i].cnt
proc used(c: Counts, i: int): bool = c.dat[i].cnt != 0
when defined hashCache:         # 2nd def triggers saving lpt behavior
  proc hash(ms: MSlice): Hash = mslice.hash(ms).uint32.Hash
  proc hash(c: var Counts, i: int, hc: Hash) {.used.} = c.dat[i].hc = hc.uint32
  proc hash(c: Counts, i: int): Hash = c.dat[i].hc.Hash
oatCounted c,Counts, c.nUsed; oatSeq Counts, dat  # make counted & resizable
when Counts is VROat[MSlice, MSlice, uint32]: {.warning: "Counts is a VROat"}

proc incFailed(h: var Counts, ms: MSlice): bool =
  if ms.len > (1 shl bLen) - 1: # Careful to not overflow
    erru "skipping too long line: ", ($ms)[0..<128], "...\n"
    return false                # Cannot go on LOCALLY
  h.upSert(ms, i):              # Found key @i:
    if h.dat[i].cnt == (1 shl bCnt) - 1:
      erru "counter overflow for: ",$ms,"\n" # no update XXX rate limit
    else: h.dat[i].cnt.inc      #   bump
  do:                           # Novel key->i:
    h.dat[i].off = s.add(ms, (1 shl bOff) - 1):
      erru "unique line data overflow at:",$ms,"\n" #XXX rate limit
      return true               # Cannot go on GLOBALLY
    h.dat[i].len = ms.len.uint32# Init
    h.dat[i].cnt = 1u32

proc lfreq(n=10, count=false, size=9999, dSize=81920, recTerm='\n',
           format="@c @k", RecTerm="\n", tm=false) =
  ## Histogram `stdin` lines (read w/non-memory mapped IO to be pipe friendly).
  ## Limits: <4 GiB unique data; <16 KiB lines; <4 GiCount.
  let t0 = if tm: epochTime() else: 0.0
  var h: Counts; h.setCap size  # pre-size table & data
  s.setLen dSize; s.setLen 0
  var nTot = 0
  block IO:
    for (line, nLine) in stdin.getDelims(recTerm):
      let ms = MSlice(mem: line, len: nLine - 1)
      inc nTot                  # Always bump `nTotal`
      if h.incFailed(ms): break IO
  if count: outu h.len," unique ",nTot," total ",s.len," B\n"
  let nInv = 1.0/nTot.float; var cs, fs: string # Setup for..
  let prs = format.tmplParsed('@')              #..nice output
  template output =
    for (id, arg, call) in prs:
      if id.idIsLiteral: outu MSlice(mem: format[arg.a].addr, len: arg.len)
      elif format[id.a] == 'c': cs.setLen 0; cs.addInt c; outu cs
      elif format[id.a] == 'k': outu k
      elif format[id.a] == 'f': fs.setLen 0; fs.fcvt c.float*nInv, 9; outu fs
      else: outu MSlice(mem: format[call.a].addr, len: call.len)
    outu RecTerm
  if   n == 0: (for (k, c) in pairs(h): output())
  elif n > 0 : (for (k, c) in h.topByVal(n): output())
  elif n < -1: (for (k, c) in h.topByVal(-n, order=Descending): output())
  if tm: stderr.write epochTime() - t0, "\n"  # -n-1 for only time output

when isMainModule: dispatch lfreq, help={
  "n"    : "emit `n`-most common  lines(0:all; <0 sorted)",
  "count": "only emit counts: unique & grand total",
  "size" : "pre-size hash table for size unique entries",
  "dSize": "pre-size str data area to this many bytes",
  "recTerm": "input record terminator",
  "RecTerm": "output record terminator",
  "format" : "output format: $k=key $c=count $f=fraction",
  "tm"   : "emit wall time of counting to stderr & quit"}
