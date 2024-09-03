## gcc/clang error out if the generated C includes a tmmintrin.h header on CPUs
## without -march=enabling the instructions.  An admittedly expensive staticExec
## lets us probe a build-time system for all pre-defined C preprocessor macros
## in one execution.  We then postprocess these into a set of flags for Nim
## compile-time `when` checks to make "fall back" easy/natural.
from strutils import contains

const ccDumpMacro {.used.} = " -dM -E -x c - </dev/null"
const ccPreDefs* =
  when defined(gcc)  : staticExec("gcc   -march=native" & ccDumpMacro)
  elif defined(clang): staticExec("clang -march=native" & ccDumpMacro)
  else: ""

type X86Feature* = enum x86sse2, x86ssse3, x86bmi2

const x86features* = static:  #XXX This obviously needs to be fleshed out
  var s: set[X86Feature]
  if " __SSE2__ "  in ccPreDefs: s.incl x86sse2
  if " __SSSE3__ " in ccPreDefs: s.incl x86ssse3
  if " __BMI2__ "  in ccPreDefs: s.incl x86bmi2
  s

# We could potentially do a calculation here to decide what march to pass.
when x86features.len > 0: {.passc: "-march=native".}
