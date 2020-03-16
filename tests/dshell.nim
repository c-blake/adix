import metab, os, strutils

proc main() =
  var lgsz = parseInt(getEnv("LGSZ", "2"))
  var nP0, nP1, nG0, nG1, nD0, nD1: int = 0
  if paramCount() > 1:
    echo("Usage:\n  ", paramStr(0), "< [gpdTPD]K [..]")
    quit(1)
  var s = initSet[int8](1 shl lgsz, rehash=false, numer=3, denom=1)
  for line in lines(stdin):                      # Dispatch operations
    let c = line[0]
    let k = int8(if line.len > 1: parseInt(line[1 .. ^1]) else: 0)
    case c
      of 'g':
        if k in s: nG1.inc
        else     : nG0.inc
      of 'p':
        if s.containsOrIncl(k): nP0.inc
        else                  : nP1.inc
      of 'd':
        if s.missingOrExcl(k): nD0.inc
        else                 : nD1.inc
      of 'T': echo s
      of 'D': echo s.depths
      of 'P': s.debugDump
      else: echo "UNKNOWN COMMAND:", c.repr; quit 2

main()
