from std/math     import sqrt
from labFloats    import labFloats, initSep
from adix/stat    import basicStats, BasicStats
from cligen/strUt import fmtUncertain
when not declared(stdin): import std/syncio

proc mnsd*(delim="white", pm="+-", unity="$val0 $pm $err0",
           sci="($valMan $pm $errV)$valExp", nd=2, exp = -2..4) =
  ## This consumes anything that looks like repeated/regular intercalary text
  ## with floating point numbers embedded and produces a one-line summary with
  ## the last such intercalary text and mean+-stderr of each *column* of floats.
  let (labs, nums) = labFloats(stdin, initSep(delim))
  strUt.pmDfl = pm
  for j, ctxt in labs:
    if nums[j].len > 1:
      let bs = basicStats(nums[j])
      if bs.sdev > 1e-14*bs.mean:           # Varied
        stdout.write fmtUncertain(bs.mean, bs.sdev/bs.n.float.sqrt,
                                  unity, sci, exp, nd)
      else: stdout.write labs[j]            # Constant
    else: stdout.write labs[j]              # Non-Numeric
  stdout.write "\n"

when isMainModule:
  import cligen
  dispatch mnsd, help={"help"       : "print cligen-erated help",
                       "help-syntax": "prepend, plurals..",
                       "delim"      : "inp delims; Repeats=>fold",
                       "pm"         : "plus|minus string",
                       "unity"      : "near unity format",
                       "sci"        : "scientific format",
                       "nd"         : "n)um sig d)igits of sigma",
                       "exp"        : "pow10 range for 'unity'"}
#XXX This should grow a 2-row output format (with labels as column headers).
