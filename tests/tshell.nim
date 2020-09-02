import metab, strutils, os, times

proc now(): int64 {.inline.} = cast[int64](epochTime() * 1e9)

proc main() =
  var verb = getEnv("VERB", "xyzpdq") != "xyzpdq"
  var size = parseInt(getEnv("SIZE", "2"))
  var nLoop0, nLoop1, nP0, nP1, nG0, nG1, nD0, nD1: int = 0
  var t0, t1, tL0, tL1: int64
  var stopped = false
  var op: seq[char] = @[]                     # operation, Key, Val seqs
  when defined(directIndex):
    var ky: seq[int8]  = @[]
  else:
    var ky: seq[int]  = @[]
  var vl: seq[int]  = @[]
  var inp: string = stdin.readAll             # Pre-read+parse to not time that
  inp.setLen inp.len - 1                      # Chop last nl; .. is inclusive
  for line in inp.split('\n'):
    let cols = line.split
    op.add cols[0][0]
    ky.add(typeof(ky[0])(if cols[0].len > 1: parseInt(cols[0][1..^1]) else: 0))
    vl.add(if cols.len > 1: parseInt(cols[1])             else: 0)
  if op.len < 1 or paramCount() > 1:
    echo "Usage:\n  ", paramStr(0), "< [gpdaTZzLl.PD]K V"
    quit 1
  var t = initTab[typeof(ky[0]), int](size, rehash=false)
  var had: bool
  t0 = now()
  for i in 0 ..< ky.len:                      # Dispatch operations
    let c = op[i]
    let k = ky[i]
    let v = vl[i]
    if verb: echo c, k, " ", v                # Verb mode helpful to trap bugs
    case c
      of 'g': (if k in t: nG1.inc else: nG0.inc)
      of 'p':
        discard t.mgetOrPut(k, v, had)
        if had: nP1.inc else: nP0.inc
      of 'd':
        t.del k, had
        if had: nD1.inc else: nD0.inc
      of '-':
        if k == 0: discard t.pop()
        else: (var kk = k; var vv = v; discard t.pop(kk, vv))
      of 'a': t.add(k, v)
      of 'T': echo t
      of 'Z': t0 = now(); nP0 = 0; nP1 = 0; nG0 = 0; nG1 = 0; nD0 = 0; nD1 = 0
      of 'z': t1 = now(); stopped = true
      of 'L': tL0 = now(); nLoop0 = i
      of 'l': tL1 = now(); nLoop1 = i
      of '.': discard                         # Just to time op dispatch ovrhead
      of 'P': t.debugDump
      of 'D': echo t.depths
      else: echo "UNKNOWN COMMAND:", c.repr; quit 2
  if not stopped: t1 = now()
  t1 -= t0
  if nLoop1 - nLoop0 > 0:
    var perDispatch = float(tL1 - tL0) / float(nLoop1 - nLoop0)
    t1 -= int64(float(nP0 + nP1 + nG0 + nG1 + nD0 + nD1) * perDispatch)
  echo "ns: ", t1, " nP1: ", nP1, " nP0: ", nP0, " nG1: ", nG1, " nG0: ", nG0,
       " nD1: ", nD1, " nD0: ", nD0, " a: ", t.len.float / t.getCap.float,
       " M: ", t.getCap
main()
