import metab, os, strutils

proc main() =
  var lgsz = parseInt(getEnv("LGSZ", "2"))
  var nP0, nP1, nG0, nG1, nD0, nD1: int = 0
  if paramCount() > 1:
    echo("Usage:\n  ", paramStr(0), "< [gpdTPD]K [..]")
    quit(1)
  var t = initTab[int8,int](1 shl lgsz, rehash=false)
  var had: bool
  for line in lines(stdin):                      # Dispatch operations
    let cols = line.split
    let c = cols[0][0]
    let k = int8(if cols[0].len > 1: parseInt(cols[0][1 .. ^1]) else: 0)
    let v = if cols.len > 1: parseInt(cols[1])             else: 0
    case c
      of 'g':
        if k in t: nG1.inc
        else     : nG0.inc
      of 'p':
        discard t.mgetOrPut(k, v, had)
        if had: nP0.inc
        else  : nP1.inc
      of 'a':
        t.add(k, v)
      of 'd':
        t.del k, had
        if had: nD1.inc
        else  : nD0.inc
      of 'T': echo t
      of 'D': echo t.depths
      of 'P': t.debugDump
      else: echo "UNKNOWN COMMAND:", c.repr; quit 2
  echo "nP1: ", nP1, " nP0: ", nP0, " nG1: ", nG1, " nG0: ", nG0,
       " nD1: ", nD1, " nD0: ", nD0
main()
