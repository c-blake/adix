when not declared(stdin): import std/[syncio, formatfloat]
import std/[hashes, times], cligen, cligen/[mslice, osUt], adix/oats

const bLen {.intdefine.} =  5   # <32B long;  RT params better but less easy
const bOff {.intdefine.} = 27   # <128MiB UNIQUE word data
type
  Count {.packed.} = object     # Dense-ish hash Count type
    when defined hashCache: hc: uint32 # 4B|8B per cell
    len {.bitsize: bLen.}: uint8
    off {.bitsize: bOff.}: uint32
  Counts = object
    dat: seq[Count]
    nUsed: int

var s: string; s.keyStack off,uint32, Count,MSlice
proc key(c: Counts, i: int): MSlice = c.dat[i].key
proc val(c: Counts, i: int): Void {.used.} = discard #NONE
proc used(c: Counts, i: int): bool = c.dat[i].len != 0
when defined hashCache:         # 2nd def triggers saving lpt behavior
  proc hash(c: Counts, i: int): Hash = c.dat[i].hc.Hash
  proc hash(c: var Counts, i: int, hc: uint32) {.used.} = c.dat[i].hc = hc
else:
  proc hash(c: Counts, i: int): Hash = c.dat[i].key.hash
  proc hash(c: var Counts, i: int, hc: Void) {.used.} = discard
Counts.useCountedCellSeq dat, nUsed

proc incFailed(h: var Counts, ms: MSlice): bool =
  if ms.len > (1 shl bLen) - 1: # Careful to not overflow
    erru "skipping too long word: ",$ms,"\n"
    return                      # Cannot go on LOCALLY
  h.getPut(i, ms, hc): discard  # Found key @i:
  do:                           # Novel key->i:
    h.dat[i].off = s.add(ms, (1 shl bOff) - 1):
      erru "unique word data overflow at:",$ms,"\n" #XXX rate limit
      return true               # Cannot go on GLOBALLY
    h.dat[i].len = ms.len.uint8 # Init

const d = " \t\r,;:.?!'\"()[]{}|<>=+-*/\\0123456789&`~$#%^"
proc wu(size=9999,dSize=81920, tm=false, Dlm="") =
  ## Count unique & total words on `stdin`. <32B long; <128 MiB unique data.
  let sep = initSep(if Dlm.len != 0: Dlm else: d)
  let t0 = if tm: epochTime() else: 0.0
  var h: Counts; h.setCap size  # pre-size table & data
  s.setLen dSize; s.setLen 0
  var nTot = 0
  block IO:
    for (line, nLine) in stdin.getDelims:
      for tok in MSlice(mem: line, len: nLine - 1).frame(sep):
        if not tok.isSep and tok.ms.len > 0:
          inc nTot              # Always bump `nTotal`
          if h.incFailed(tok.ms): break IO
  echo h.len," unique ",nTot," total ",s.len," B"
  if tm: stderr.write epochTime() - t0, "\n"

when isMainModule: dispatch wu, help={
  "size" : "pre-size hash table for size unique entries",
  "dSize": "pre-size str data area to this many bytes",
  "tm"   : "emit wall time of counting to stderr & quit",
  "Dlm":"""chars by which words inside lines are delimited
""=>SPC,;:.?!'"()[]{}|<>=+-\*/\\0123456789&`~$#%^"""}
