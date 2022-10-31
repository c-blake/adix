import strutils
when not declared(stdin): import std/syncio

type                                    # Instantiate a tree type/suite
  Command* = object
    letter*: char
    sided*, side*: bool
    key*, val*: int

proc preproc*(i="/dev/stdin", o="/dev/stdout") =
  ## This program pre-parses human understandable ops for search structure
  ## shells.  Output is flushed as soon as ready so that ppss|dsshell can be
  ## used interactively at a terminal.  The language interpreted here is simple:
  ## just <letter>[0|1][<space><key>[<space><val>]] { [] denotes optionality }.
  ## This factoring allows shells to have negligible dispatch overhead/string
  ## handling and so be appropriate for benchmarks/timing experiments.
  let fi = if i == "/dev/stdin" : stdin  else: open(i, fmRead)
  let fo = if o == "/dev/stdout": stdout else: open(o, fmWrite)
  var cout: Command
  for buf in lines(fi):
    if buf.len == 0 or buf[0] == '#': continue
    let cols = buf.split
    if cols.len < 1 or cols[0].len < 1: continue
    cout.letter = cols[0][0]
    cout.sided = cols[0].len > 1
    cout.side = if cout.sided and cols[0][1] == '1': true else: false
    cout.key = int16(if cols.len > 1: parseInt(cols[1]) else: -99)
    cout.val = int16(if cols.len > 2: parseInt(cols[2]) else: 0)
    discard fo.writeBuffer(cout.addr, cout.sizeof)
    fo.flushFile

proc readObject*(f: File, buffer: pointer, size: Natural): int {.inline.} =
  proc c_fread(buf: pointer, size, n: culong, f: File): culong {.
    importc: "fread_unlocked", header: "<stdio.h>" .}
  result = int(c_fread(buffer, cast[culong](size), 1, f))

when isMainModule:
  import cligen; dispatch(preproc)
