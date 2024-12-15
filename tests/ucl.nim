when not declared(stdin): import std/[syncio, formatfloat]
import std/[hashes, times], cligen, cligen/[mslice, osUt], adix/oats

const bLen {.intdefine.} =  8   # <256 long; RT limits nicer but harder
const bOff {.intdefine.} = 24   # <16MiB UNIQUE line data
type
  Count {.packed.} = object     # Dense-ish hash Count type
    when defined hashCache: hc: uint32 # 4B|8B per cell
    len {.bitsize: bLen.}: uint32
    off {.bitsize: bOff.}: uint32
  Counts = object
    dat: seq[Count]
    nUsed: int

var s = " "; oatKStack s, Counts, Count, off,uint32, MSlice, MSlice
proc key(c: Counts, i: int): MSlice = c.dat[i].key
proc used(c: Counts, i: int): bool = c.dat[i].off!=0
when defined hashCache:                           # def auto-triggers use
  proc hash(ms: MSlice): Hash = mslice.hash(ms).uint32.Hash
  proc hash(c: var Counts, i: int, hc: Hash) {.used.} = c.dat[i].hc = hc.uint32
  proc hash(c: Counts, i: int): Hash = c.dat[i].hc.Hash
oatCounted c,Counts, c.nUsed; oatSeq Counts, dat  # make counted & resizable
#when Counts is ROat[MSlice, MSlice]: {.warning: "Counts is a ROat"}

proc incFailed(h: var Counts, ms: MSlice): bool =
  var ms = ms
  if ms.len > (1 shl bLen) - 1: # Careful to not overflow XXX rate limit msgs
    erru "truncating too long (", $ms.len, ") line: ", ($ms)[0..<256], "...\n"
    ms.len = (1 shl bLen) - 1   # Truncation makes count potentially off
  h.upSert(ms, i): discard      # Found key @i: nothing to do
  do:                           # Novel key->i:
    h.dat[i].off = s.add(ms, (1 shl bOff) - 1):
      erru "unique word data overflow at:",$ms,"\n" #XXX rate limit msgs
      return true               # Cannot go on GLOBALLY
    h.dat[i].len = ms.len.uint32 # Init

proc ucl(size=9999, dSize=81920, tm=false) =
  ## Count unique & total lines on `stdin`. <256B long; <16 MiB unique data.
  let t0 = if tm: epochTime() else: 0.0
  var h=Counts(); h.setCap size # pre-size table & data
  s.setLen dSize; s.setLen 1
  var nTot = 0
  block IO:
    for (line, nLine) in stdin.getDelims:
      let ms = MSlice(mem: line, len: nLine - 1)
      inc nTot                  # Always bump `nTotal`
      if h.incFailed(ms): break IO
  echo h.len," unique ",nTot," total ",s.len," B"
  if tm: stderr.write epochTime() - t0, "\n"

when isMainModule: dispatch ucl, help={
  "size" : "pre-size hash table for size slots",
  "dSize": "pre-size str data area to this many bytes",
  "tm"   : "emit wall time of counting to stderr & quit"}
