from std/math     import sqrt
from labFloats    import labFloats, initSep
from adix/stat    import basicStats, BasicStats
from cligen/strUt import fmtUncertain
from std/strutils import join, strip
when not declared(stdin): import std/syncio

proc mnsd*(delim="white", table="", hsep="s", pm="+-", exp = -2..4,
           unity="$val0 $pm $err0", sci="($valMan $pm $errV)$valExp", nd=2) =
  ## This consumes any stdin looking like repeated/regular intercalary text with
  ## numbers embedded & produces a 1-line summary with the last such intercalary
  ## text and mean+-stderr of each *column* of floats.  If `table!=""`, contexts
  ## are joined via `hsep` into headers for the associated reduced numbers, with
  ## columns separated by `table` (e.g. ',').
  let (labs, nums) = labFloats(stdin, initSep(delim))
  strUt.pmDfl = pm
  if table.len > 0:
    # TEST: regularTrailing regularLeading allNumbers irregTrailing irregLeading
    var hdr: string
    var hdrs, cols: seq[string]
    let doStrip = hsep == "s"
    let hsep = if doStrip: "" else: hsep
    for j, ctxt in labs:
      if nums[j].len > 1:
        if hdr.len > 0:
          hdrs.add if doStrip: hdr.strip else: hdr
          hdr.setLen 0
        let bs = nums[j].basicStats
        if bs.sdev > 1e-14*bs.mean:   # Varied
          cols.add fmtUncertain(bs.mean, bs.sdev/bs.n.float.sqrt,
                                unity, sci, exp, nd)
        else: cols.add ctxt           # Constant
      else:                           # Non-Numeric
        if hdr.len > 0: hdr.add hsep
        hdr.add ctxt
    if hdr.len > 0:                   # Last header may be empty
      hdrs.add if doStrip: hdr.strip else: hdr
    stdout.write join(hdrs, table), "\n"
    stdout.write join(cols, table), "\n"
  else:
    for j, ctxt in labs:
      if nums[j].len > 1:
        let bs = nums[j].basicStats
        if bs.sdev > 1e-14*bs.mean:     # Varied
          stdout.write fmtUncertain(bs.mean, bs.sdev/bs.n.float.sqrt,
                                    unity, sci, exp, nd)
        else: stdout.write ctxt      # Constant
      else: stdout.write ctxt        # Non-Numeric
    stdout.write "\n"

when isMainModule: import cligen; dispatch mnsd, help={
  "help"       : "print cligen-erated help",  # narrower than
  "help-syntax": "prepend, plurals..",        #..the defaults
  "delim"      : "inp delims; Repeats=>fold",
  "table"     :"""labels -> header of a
`table`-separated table""",
  "hsep"       : "header sep | strip if==s",
  "pm"         : "plus|minus string",
  "unity"      : "near unity format",
  "sci"        : "scientific format",
  "nd"         : "n)um sig d)igits of sigma",
  "exp"        : "pow10 range for 'unity'"}
