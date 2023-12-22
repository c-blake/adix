when not declared(stdin): import std/[syncio, formatfloat]
import std/[hashes, times], cligen, cligen/[mslice, osUt], adix/oats

const bLen {.intdefine.} = 10   # <1024 long; RT limits nicer but harder
const bOff {.intdefine.} = 22   # <4MiB UNIQUE line data
type
  Count {.packed.} = object     # Dense-ish hash Count type
    when defined hashCache: hc: uint32 # 4B|8B per cell
    len {.bitsize: bLen.}: uint32
    off {.bitsize: bOff.}: uint32
  Counts = object
    dat: seq[Count]
    nUsed: int

var a = " "; oatKStack a, Counts, Count, off,uint32, MSlice, MSlice
#proc key(c: var Counts, i: int, q: MSlice) = c.dat[i]=c.keyR(q) wrong&unneeded
proc key(c: Counts, i: int): MSlice = c.dat[i].key
proc used(c: Counts, i: int): bool = c.dat[i].off!=0

when defined hashCache:                           # def auto-triggers use
  proc hash(ms: MSlice): Hash = mslice.hash(ms).uint32.Hash
  proc hash(c: var Counts, i: int, hc: Hash) {.used.} = c.dat[i].hc = hc.uint32
  proc hash(c: Counts, i: int): Hash = c.dat[i].hc.Hash

oatCounted c,Counts, c.nUsed; oatSeq Counts, dat  # make counted & resizable
when Counts is ROat[MSlice, MSlice]: {.warning: "Counts is a ROat"}

proc incFailed(h: var Counts, r: MSlice): bool =
  if r.len + 1 > 1 shl bLen:    # Careful to not overflow
    erru "skipping too long(", $r.len, ") line: ",$r,"\n"
    return                      # Cannot go on LOCALLY
  h.upSert(r, i): discard       # Found key @i: nothing to do
  do:                           # Novel key->i:
    h.dat[i].off = a.add(r, (1 shl bOff) - 1):
      erru "unique word data overflow at:",$r,"\n" #XXX rate limit msgs
      return true               # Cannot go on GLOBALLY
    h.dat[i].len = r.len.uint32 # Init

proc ucl(size=9999, dSize=81920, tm=false) =
  ## Count unique & total lines on `stdin`. <256B long; <16 MiB unique data.
  let t0 = if tm: epochTime() else: 0.0
  var h: Counts; h.setCap size  # Pre-size table & data
  a.setLen dSize; a.setLen 1
  var nTot = 0
  block IO:
    for (line, nLine) in stdin.getDelims:
      let ms = MSlice(mem: line, len: nLine - 1)
      inc nTot                  # Always bump `nTotal`
      if h.incFailed(ms): break IO
  echo h.len," unique ",nTot," total ",a.len," B"
  if tm: stderr.write epochTime() - t0, "\n"

when isMainModule: dispatch ucl, help={
  "size" : "pre-size hash table for size slots",
  "dSize": "pre-size str data area to this many bytes",
  "tm"   : "emit wall time of counting to stderr & quit"}
