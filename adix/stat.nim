## Summary stats built in running/online fashion (as std/stats) BUT over (maybe)
## MOVING data windows (via `pop`) and (sometimes) 50X faster & a million X more
## accurate.  Speed up comes from SIMD auto-vectorization in whole `openArray[]`
## calls (in --passC:-ffast-math|-Ofast backend modes) aided by "shift" idea at
## en.wikipedia.org/wiki/Algorithms_for_calculating_variance (both simpler &
## faster than Welford).  Both `var` & non-var overloads are provided to allow
## caching 1.0/n which may be identical-but-expensive (eg. reporting at each
## cycle of a 1 pop per push (so fixed `n`) window over data).
##
## Note: this all costs more in both time & space than exponentially weighted
## equivalents but has precise rather than infinite memory which can be nice.
## I.e., it can perfectly "forget" a large spike when it leaves a window.

from math         import sqrt, sum, `^`
from strutils     import formatFloat, ffDefault
when defined(useCligen): from cligen/strUt import fmtUncertainMerged
else:
  when not compiles($1.0): import std/formatfloat
  proc fmtUncertainMerged(m, e: float): string = $m & " +- " & $e
from adix/lghisto import LgHisto, add, pop, quantile, cdf, init

const avc = "__attribute__((optimize(\"fast-math\"))) $# $#$#" # autovec cgdecl
type
  Option* = enum OrderStats
  MovingStat*[F:SomeFloat,C:SomeInteger] = object ##Statistical data accumulator
    options*: set[Option]
    n*, n4Inv: int                    ## amount of pushed data
    nIn, dx, s1, s2, s3, s4: F        # 1/n, shift, sums of k-th pows of inputs
    min*, max*: F
    lgHisto*: LgHisto[C]
  BasicStats*[F: SomeFloat] = object  ## Result type for parallel-mergable stats
    n*: int                           ## sample size
    min*, max*, mean*, sdev*: F       ## the usual suspects.

func init*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C], a=1e-16,b=1e20,
            n=8300, options: set[Option]={}) {.inline.} =
  ## Initialized a `MovingStat[F,C]` with a [-b,b] log-spaced histogram.
  s.options = options
  if OrderStats in options: s.lgHisto.init a, b, n

func initMovingStat*[F: SomeFloat, C: SomeInteger](a=1e-16, b=1e20, n=8300,
                       options: set[Option]={}): MovingStat[F,C] {.inline.} =
  ## Return an initialized `MovingStat[F,C]` with a [-b,b] log-spaced histogram.
  result.init a, b, n, options

func nInv*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]): F {.inline.} =
  ## A reciprocal caching `1.0/s.n`.
  if s.n4Inv != s.n:
    s.n4Inv = s.n
    s.nIn = 1.0/F(s.n)
  s.nIn

func nInv*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]): F {.inline.} =
  ## A reciprocal caching `1.0/s.n`; cannot update reciprocal.
  if s.n4Inv != s.n: 1.0/F(s.n) else: s.nIn

func clear*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]) {.inline.} =
  ## Reset `s` to same as `var s: MovingStat[F,C]`.
  zeroMem s.addr, s.sizeof

func push*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C],
            x: SomeNumber) {.inline, codegenDecl: avc.} =
  ## Pushes a value `x` into running sums.
  let x0 = F(x)
  if s.n == 0: s.min = x0; s.max = x0; s.dx = x0
  else: s.min = min(s.min, x0); s.max = max(s.max, x0)
  let x  = x0 - s.dx
  let x2 = x*x
  s.s1 = s.s1 + x
  s.s2 = s.s2 + x2
  s.s3 = s.s3 + x2*x
  s.s4 = s.s4 + x2*x2
  inc s.n
  if OrderStats in s.options: s.lgHisto.add x0

func pop*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C],
           x: SomeNumber) {.inline, codegenDecl: avc.} =
  ## Pops (aka removes the influence) of a value `x` from running sums.
  let x  = F(x) - s.dx
  let x2 = x*x
  s.s1 = s.s1 - x
  s.s2 = s.s2 - x2
  s.s3 = s.s3 - x2*x
  s.s4 = s.s4 - x2*x2
  dec s.n
  if OrderStats in s.options: s.lgHisto.pop x

func quantile*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C],
                q: float): float {.inline.} =
  ## Estimated moving quantile `q`; E.g. `q=0.5` is the moving median.
  if OrderStats in s.options: s.lgHisto.quantile(q)
  else: NaN

func cdf*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C],
           x: float): float {.inline.} =
  ## Estimated moving CDF(x)
  if OrderStats in s.options: s.lgHisto.cdf(x)
  else: NaN

func sum*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]): float {.inline.} =
  ## Moving sum/total over data window.
  s.s1 + s.n.float*s.dx.float

func mean*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]): float {.inline.} =
  ## Moving mean/average over data window.
  s.s1*s.nInv + s.dx
func mean*[F:SomeFloat,C:SomeInteger](s: var MovingStat[F,C]): float {.inline.}=
  ## Moving mean/average over data window.
  s.s1*s.nInv + s.dx

func variance*[F:SomeFloat,C:SomeInteger](s: MovingStat[F,C]): float {.inline.}=
  ## Moving variance (population) over data window.
  max(1e-30*s.mean^2, (s.s2 - s.s1*s.s1*s.nInv)*s.nInv) # sqrt=1e-15
func variance*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]):
                float {.inline.} =
  ## Moving variance (population) over data window.
  max(1e-30*s.mean^2, (s.s2 - s.s1*s.s1*s.nInv)*s.nInv) # sqrt=1e-15

func standardDeviation*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]):
                         float {.inline.} = s.variance.sqrt
  ## Moving standard deviation (population) over data window.
func standardDeviation*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]):
                         float {.inline.} =
  ## Moving standard deviation (population) over data window.
  s.variance.sqrt

func stderror*[F:SomeFloat,C:SomeInteger](s: MovingStat[F,C]): float {.inline.}=
  ## Moving standard error (standard deviation of the mean) over data window.
  sqrt(s.variance*s.nInv)
func stderror*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]):
                float {.inline.} =
  ## Moving standard error (standard deviation of the mean) over data window.
  sqrt(s.variance*s.nInv)

template skewImpl =
  let s1 = s.s1*s.nInv; let s2 = s.s2*s.nInv; let s3 = s.s3*s.nInv
  let scl = F(1)/s.standardDeviation
  result = (s3 - 3*s1*s2 + 2*s1*s1*s1)*scl*scl*scl
func skewness*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]):
                float {.inline.} = skewImpl
  ## Moving skewness (population) over data window.
func skewness*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]):
                float {.inline.} = skewImpl
  ## Moving skewness (population) over data window.

template kurtImpl =
  let s1 = s.s1*s.nInv; let s2 = s.s2*s.nInv
  let s3 = s.s3*s.nInv; let s4 = s.s4*s.nInv
  let scl = F(1)/s.standardDeviation
  let c2 = scl*scl
  result = (s4 - 4*s1*s3 + 6*(s1*s1)*s2 - 3*(s1*s1)*(s1*s1))*c2*c2
  result -= F(3) # Gaussian-relative excess kurtosis
func kurtosis*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]):
                float {.inline.} = kurtImpl
  ## Moving excess-relative to Gaussian kurtosis (population) over data window.
func kurtosis*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]):
                float {.inline.} = kurtImpl
  ## Moving excess-relative to Gaussian kurtosis (population) over data window.

proc `$`*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]): string =
  ## A string representation of a `MovingStat[F,C]`.
  let skew = s.skewness.float
  let kurt = s.kurtosis.float
  fmtUncertainMerged(s.mean.float, s.stderror.float) &
  " [ " & $(s.min) & " .. " & $(s.max) & " ]" &
  " sd: " & formatFloat(s.standardDeviation.float, ffDefault, 4) &
  " sk: " & formatFloat(skew, ffDefault, 4) &
  " ek: " & formatFloat(kurt, ffDefault, 3)

func add*[F](a: var BasicStats[F], b: BasicStats[F]) =
  ## Combines two `BasicStat`s.  Useful to combine partial result sets.
  a.max = max(a.max, b.max)
  a.min = min(a.min, b.min)
  let am = a.mean; let bm = b.mean; let nC = a.n + b.n    # a,b,combined n
  let cm = (am*a.n.F + bm*b.n.F)/nC.F                     # combined mean
  let aM2 = F(a.n)*a.sdev*a.sdev; let bM2 = F(b.n)*b.sdev*b.sdev
  let cvcn = aM2 + a.n.F*(am - cm)^2 + bM2 + b.n.F*(bm - cm)^2
  a.mean = cm
  a.sdev = sqrt(cvcn / F(nC))
  a.n    = nC
func `+=`*[F](a: var BasicStats[F], b: BasicStats[F]) = a.add b
  ## Operator notation for `add`.
func `+`*[F](a, b: BasicStats[F]): BasicStats[F] = result = a; result.add b
  ## Operator notation for `add`.

func mean*[T: SomeNumber](xs: openArray[T]): float {.codegenDecl:avc.} =
  xs.sum.float/xs.len.float
  ## Arithmetic mean/average; Used `math.sum` can overflow for narrow types.

func variance*[T:SomeNumber](xs:openArray[T],accum=32):float{.codegenDecl:avc.}=
  ## variance (population). `accum` != 32 => 64-bit accumulation.
  template impl(F) =
    var m, v: F
    let dx = if xs.len > 0: xs[0] else: F(0)
    for x in xs:
      let x = F(x) - dx
      m = m + x
      v = v + x*x
    m = m.float/xs.len.float
    result = F(v.float/xs.len.float - m.float*m.float)
  if accum == 32: impl(float32) else: impl(float64)

func standardDeviation*[T: SomeNumber](xs: openArray[T], accum=32): float =
  ## standard deviation (population). `accum` != 32 => 64-bit accumulation.
  xs.variance(accum).sqrt

func stderror*[T: SomeNumber](xs: openArray[T], accum=32): float =
  ## standard error (std dev of the mean). `accum` != 32 => 64-bit accumulation.
  sqrt(xs.variance(accum)/xs.len.float)

func skewness*[T:SomeNumber](xs:openArray[T],accum=32):float{.codegenDecl:avc.}=
  ## skewness (population). `accum` != 32 => 64-bit accumulation.
  template impl(F) =
    var s1, s2, s3: F
    let dx = if xs.len > 0: xs[0] else: F(0)
    let nInv = F(1)/F(xs.len)
    for x in xs:
      let x = F(x) - dx
      s1 = s1 + x
      s2 = s2 + x*x
      s3 = s3 + x*x*x
    s1 = s1*nInv; s2 = s2*nInv; s3 = s3*nInv
    let scl = F(1)/sqrt(max(1e-30*s1^2, s2 - s1*s1))
    result = (s3 - 3*s1*s2 + 2*s1*s1*s1)*scl*scl*scl
  if accum == 32: impl(float32) else: impl(float64)

func kurtosis*[T:SomeNumber](xs:openArray[T],accum=32):float{.codegenDecl:avc.}=
  ## excess kurtosis (population). `accum` != 32 => 64-bit accumulation.
  template impl(F) =
    var s1, s2, s3, s4: F
    let dx = if xs.len > 0: xs[0] else: F(0)
    let nInv = F(1)/F(xs.len)
    for x in xs:
      let x = F(x) - dx
      s1 = s1 + x
      s2 = s2 + x*x
      s3 = s3 + x*x*x
      s4 = s4 + x*x*x*x
    s1 = s1*nInv; s2 = s2*nInv; s3 = s3*nInv; s4 = s4*nInv
    let scl = F(1)/sqrt(max(1e-30*s1^2, s2 - s1*s1))
    let c2 = scl*scl
    result = (s4 - 4*s1*s3 + 6*(s1*s1)*s2 - 3*(s1*s1)*(s1*s1))*c2*c2
    result -= F(3) # Gaussian-relative excess kurtosis
  if accum == 32: impl(float32) else: impl(float64)

# "Sample" variants are included for compatibility with std/stats, but will stay
# undocumented & unoptimized since they mislead more than help.  A) generalizing
# from samples to parent pops is nearly always a stretch, B) >1st mom estimators
# are notoriously noisy (statistically) C) adj is INCORRECT for SD; distribution
# free adj also does not exist (en.wikipedia.org/wiki/Bessel%27s_correction).
template vAdj(v,n,D)=(if n > 1: (D(n) / D(n - 1))*v else: 0.0)
template sAdj(s,n,D)=(if n > 2: sqrt((n.D*D(n-1)))/D(n-2)*s else: 0.0)
template kAdj(k,n,D)=(if n > 3: D(n-1)/D((n-2)*(n-3))*(D(n+1)*k + 6) else: 0.0)
template inst(mom, adj) =
  func `mom S`*[F: SomeFloat, C: SomeInteger](s: MovingStat[F,C]): float =
    adj(s.mom, s.n, float)
  func `mom S`*[F: SomeFloat, C: SomeInteger](s: var MovingStat[F,C]): float =
    adj(s.mom, s.n, float)
  func `mom S`*[T: SomeNumber](xs: openArray[T], accum=32): float =
    adj(xs.mom(accum), xs.len, float)
inst(variance, vAdj); inst(skewness, sAdj); inst(kurtosis, kAdj)
func standardDeviationS*[F:SomeFloat,C:SomeInteger](s: MovingStat[F,C]): float =
  s.varianceS.sqrt
func standardDeviationS*[F:SomeFloat,C:SomeInteger](s: var MovingStat[F,C]):
                          float = s.varianceS.sqrt
func standardDeviationS*[T: SomeNumber](xs: openArray[T], accum=32): float =
  xs.varianceS.sqrt

func basicStats*[F:SomeFloat](xs:openArray[F]):BasicStats[F]{.codegenDecl:avc.}=
  ## The summary stats you usually want in one pass (in native FP arith).
  result.n = xs.len     # --passC:-ffast-math can autovectorize the whole loop
  if result.n > 0:      #..into vminp[sd]/vmaxp[sd]/vsubp[sd]/vaddp[sd] insns
    result.min = xs[0]  #..which for sse/avx/avx2/avx512 can really zoom.
    result.max = xs[0]         
    let nInv = F(1)/F(result.n)
    let dx = if result.n > 0: xs[0] else: F(0)
    var av, vr: F
    for x in xs:
      result.min = min(result.min, x) # min/max get autovect in equiv C but do
      result.max = max(result.max, x) #..not in Nim with -fno-finite-math-only.
      let x = x - dx
      av = av + x
      vr = vr + x*x
    av = av * nInv; vr = vr * nInv
    result.mean = F(av.float + dx)
    result.sdev = F(sqrt(max(1e-30*av^2, vr - av*av)))
  else:
    result.min  = F(NaN); result.max  = F(NaN)
    result.mean = F(NaN); result.sdev = F(NaN)

when isMainModule:
#[from mpmath import *; mp.dps = 64; mp.pretty = True
  def diffPowAvg(xs, m, k=2, n=1): return sum((mpf(x) - m)**k for x in xs)/n
  def moments(xs):             # This is a Python arbitrary precision program
    n  = mpf(len(xs))          #..to get exact answers for reference.
    m  = diffPowAvg(xs, mpf(0), 1, n)
    v  = diffPowAvg(xs, m     , 2, n)
    m3 = diffPowAvg(xs, m     , 3, n)
    m4 = diffPowAvg(xs, m     , 4, n)
    return (m, v, sqrt(v), m3/sqrt(v*v*v), m4/(v*v) - mpf(3))
  if __name__ == '__main__':
    import sys; (m,v,sd,sk,kt) = moments(sys.argv[1:]); print("avg: ", m)
    print("var: ", v); print("sd: ", sd); print("sk: ", sk); print("kt: ", kt)]#
  import std/stats
  var w1: MovingStat[float32, uint32]
  var w2: MovingStat[float64, uint32]
  var rs, rA, rB: RunningStat # Below: std/stats runnableExamples +10_000_000
  let xs = [10000001'f32, 10000002.0, 10000001.0, 10000004.0,
            10000001.0  , 10000004.0, 10000001.0, 10000002.0]
  let exactMean = 10000002.0
  let exactVar  = 1.5
  let exactSdev = 1.2247448713915890490986
  let exactSkew = 0.8164965809277260327324
  let exactKurt = -1.0
  proc selfError(x, exact: float): string = $x & "\trelErr: " & $(x/exact - 1.0)
  for x in xs: w1.push x    # Compare incremental/online & all at once results.
  for x in xs: w2.push x    # Compare incremental/online & all at once results.
  for x in xs: rs.push x
  echo "Single Precision Moving"
  echo "  mean:  ", selfError(w1.mean    , exactMean)
  echo "  vrnc:  ", selfError(w1.variance, exactVar)
  echo "  sdev:  ", selfError(w1.standardDeviation, exactSdev)
  echo "  skew:  ", selfError(w1.skewness, exactSkew)
  echo "  kurt:  ", selfError(w1.kurtosis, exactKurt)
  echo "  $: ", w1
  echo "\nDouble Precision Moving"
  echo "  mean:  ", selfError(w2.mean    , exactMean)
  echo "  vrnc:  ", selfError(w2.variance, exactVar)
  echo "  sdev:  ", selfError(w2.standardDeviation, exactSdev)
  echo "  skew:  ", selfError(w2.skewness, exactSkew)
  echo "  kurt:  ", selfError(w2.kurtosis, exactKurt)
  echo "  $: ", w2
  echo "\nArray, 32-bit accum"
  echo "  basic: ", xs.basicStats
  echo "  vrnc:  ", selfError(xs.variance, exactVar)
  echo "  skew:  ", selfError(xs.skewness, exactSkew)
  echo "  kurt:  ", selfError(xs.kurtosis, exactKurt)
  echo "\nArray, 64-bit accum"
  var x64: seq[float]
  for x in xs: x64.add x
  echo "  basic: ", x64.basicStats
  echo "  vrnc:  ", selfError(xs.variance(accum=64), exactVar)
  echo "  skew:  ", selfError(xs.skewness(accum=64), exactSkew)
  echo "  kurt:  ", selfError(xs.kurtosis(accum=64), exactKurt)
  echo "\nRunningStat (Welford/Knuth Online Algo, 64-bit):"
  echo "  mean:  ", selfError(rs.mean    , exactMean)
  echo "  vrnc:  ", selfError(rs.variance, exactVar)
  echo "  skew:  ", selfError(rs.skewness, exactSkew)
  echo "  kurt:  ", selfError(rs.kurtosis, exactKurt)
  for x in xs[0..4]: rA.push x
  for x in xs[5..7]: rB.push x
  rA += rB; echo "\nRunningStat Merged"
  echo "  mean:  ", selfError(rA.mean    , exactMean)
  echo "  vrnc:  ", selfError(rA.variance, exactVar)
  echo "  sdev:  ", selfError(rA.standardDeviation, exactSdev)
  var bA = x64[0..4].basicStats
  bA.add x64[5..7].basicStats
  echo "\nArray 64-bit accum merged: "
  echo "  ", bA
  echo "  mean:  ", selfError(bA.mean  , exactMean)
  echo "  vrnc:  ", selfError(bA.sdev^2, exactVar)
  echo "  sdev:  ", selfError(bA.sdev  , exactSdev)
  echo "MOVING(3):"
  var wM: MovingStat[float64, uint32]
  for i, x in xs:
    if i >= 3: wM.pop xs[i-3]
    wM.push x # only print for full 3windows
    if i >= 2: echo "  ",i," ",fmtUncertainMerged(wM.mean,wM.stderror,e0= -2..7)
