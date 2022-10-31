when not declared(stdin): import std/[syncio, objectdollar]
import metab, strutils, os, times

proc now(): int64 {.inline.} = cast[int64](epochTime() * 1e9)

proc main() =
  var verb = getEnv("VERB", "xyzpdq") != "xyzpdq"
  var nLoop0, nLoop1, nP0, nP1, nG0, nG1, nD0, nD1: int = 0
  var t0, t1, tL0, tL1: int64
  var stopped = false
  var op: seq[char] = @[]                     # operation & Key sequences
  when defined(directIndex):
    var size = parseInt(getEnv("SIZE", "0"))
    var ky: seq[int8]  = @[]
  else:
    var size = parseInt(getEnv("SIZE", "2"))
    var ky: seq[int]  = @[]
  var inp: string = stdin.readAll             # Pre-read+parse to not time that
  inp.setLen inp.len - 1                      # Chop last nl; .. is inclusive
  for line in inp.split('\n'):
    op.add line[0]
    ky.add(typeof(ky[0])(if line.len > 1: parseInt(line[1 .. ^1]) else: 0))
  if op.len < 1 or paramCount() > 1:
    echo("Usage:\n  ", paramStr(0), "< [gpdTZzLl.]K [..]")
    quit(1)
  var s = initSet[typeof(ky[0])](size, rehash=false, numer=3, denom=1)
  t0 = now()
  for i in 0 ..< ky.len:                      # Dispatch operations
    let c = op[i]
    let k = ky[i]
    if verb: echo c, k                        # Verb mode helpful to trap bugs
    case c
      of 'a': s.add k
      of 'g': (if k in s: nG1.inc else: nG0.inc)
      of 'p': (if s.containsOrIncl(k): nP0.inc else: nP1.inc)
      of 'd': (if s.missingOrExcl(k): nD0.inc else: nD1.inc)
      of '-':
        if k == 0: discard s.pop()
        else: (var kk = k; discard s.pop(kk))
      of 'T': echo s
      of 'Z': t0 = now(); nP0 = 0; nP1 = 0; nG0 = 0; nG1 = 0; nD0 = 0; nD1 = 0
      of 'z': t1 = now(); stopped = true
      of 'L': tL0 = now(); nLoop0 = i
      of 'l': tL1 = now(); nLoop1 = i
      of '.': discard                         # Just to time op dispatch ovrhead
      of 'P': s.debugDump
      of 'D': echo s.depths
      else: echo "UNKNOWN COMMAND:", c.repr; quit 2
  if not stopped: t1 = now()
  t1 -= t0
  if nLoop1 - nLoop0 > 0:
    var perDispatch = float(tL1 - tL0) / float(nLoop1 - nLoop0)
    t1 -= int64(float(nP0 + nP1 + nG0 + nG1 + nD0 + nD1) * perDispatch)
  echo "nP1: ", nP1, " nP0: ", nP0, " nG1: ", nG1, " nG0: ", nG0,
       " nD1: ", nD1, " nD0: ", nD0
main()
