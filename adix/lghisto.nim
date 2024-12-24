## `LgHisto` is a simple application of BISTs to log-spaced histograms that can
## yield efficient, dynamic quantiles.  log-spacing supports high dynamic range
## without inordinate cost while Fenwick/BIST supports dynamic membership with
## operation-balanced perf.
##
## Quantile error is absolute {not relative to `q*(1-q)` like a t-Digest} &
## easily bounded as <~ 1/2 bin width {about 10^(lg(b/a)/n/2)}.  So, if you need
## 3 places or your data is clustered within a few orders of magnitude then you
## can likely just use 1e4 bins and your counters will remain very L1 cache
## resident, depending upon resource competition.  Cache is the main cost Re:
## speed.  Re: space, since 99% of bins are 0 in many cases, net/disk transfer
## cost can be improved via simple run-length encoding.
##
## The way Fenwick BISTs work, the generic parameter `C` must be a wide enough
## integer type to hold both elemental bin counts AND grand totals.  uint32 is
## likely enough for many applications, though some might sneak by with uint16
## and a few might need uint64.  This scales bin size/space cost.
##
## t-Digests are a well marketed competitor using ~10X less space BUT with >>5X
## slower quantiles of similar accuracy.  Actual cost is sensitive to operation
## mixes. { This may change, but present t-Digest impls, even with trees, linear
## scan for quantile/CDFs.  None even offer "batch" APIs to do N quantiles in
## one such scan.  "Histo B-trees" should allow better scaling for such. }  A
## BIST basis differs from t-Digests in other important ways.  First, BISTs are
## well suited for `pop` or moving data window operations with *strict* finite
## memory, for e.g. translation of full streams to moving quantiles as in
## Bollinger Band style smooths.  Second, floating point weights for EWMA-like
## decaying memory are not possible since FP arithmetic kind of breaks BISTs.

{.warning[Uninit]:off, warning[ProveInit]:off.} # Should be verbosity:2, not 1
when not declared(addFloat): import std/formatfloat
import adix/[bist, lna]; from std/math import exp, isNaN
type
  LgHisto*[C] = object  ## Log-spaced histogram with `Bist[C]` backing
    n: int              # number of bins
    a, b: float         # histogram covers [-b, -a], (-a, a) in zero, [a, b]
    aLn, h, hInv: float # index scale conversion pre-computes
    bist: Bist[C]       # actual smart array of counters: [0, 2*n] -> PMF/CDF
#2DO^ Fastr flat array option(cumsum for "final" quantiles); Lowr prec `ln`=>DDS
#Could also take an option like `noNegative`, but untouched cache matters little

func underflows*[C](s: LgHisto[C]): int = s.bist.pmf 0
func overflows*[C](s: LgHisto[C]): int  = s.bist.pmf 2*s.n
func low*[C](s: LgHisto[C]): float      = s.a
func high*[C](s: LgHisto[C]): float     = s.b
func nBin*[C](s: LgHisto[C]): int       = s.n
func bist*[C](s: LgHisto[C]): Bist[C]   = s.bist

func init*[C](s: var LgHisto[C], a=1e-16, b=1e20, n=8300) =
  ## Init histo w/2n+1 log-spaced bins: `[-∞..-b; -b..-a; 0; a..<b; b..∞]`.
  if b <= a: raise newException(ValueError, "inverted: [" & $a & "," & $b & "]")
  if a <= 0.0 or b <= 0.0: raise newException(ValueError, "a,b must both be >0")
  if n < 2: raise newException(ValueError, "n must >= 2")
  s.n    = n
  s.a    = a
  s.b    = b
  s.aLn  = lna(a)
  s.h    = (lna(b) - lna(a))/float(n - 1)
  s.hInv = 1.0/s.h
  s.bist = initBist[C](2*n + 1)

func initLgHisto*[C](a=1e-16, b=1e20, n=8300): LgHisto[C] = result.init a, b, n
  ## Get Histo w/2n+1 log-spaced bins: `[-inf..<-b; -b..<-a; 0; a..<b; b..inf]`.

func space*[C](s: LgHisto[C]): int = s.sizeof + s.bist.space
  ## Estimate space taken up by data structure in bytes

func toIx*[F,C](s: LgHisto[C], x: F): int =
  ## Find bin index for value `x`; underflows get `[0]` & overflows get `[2*n]`.
  if   x <= -s.a:
    if x >= -s.b: result = s.n - 1 - int( (lna(-x) - s.aLn)*s.hInv)
    else        : result = 0
  elif x >= +s.a:
    if x <= +s.b: result = s.n + 1 + int( (lna(+x) - s.aLn)*s.hInv)
    else        : result = 2*s.n
  else: result = s.n

func fromIx*[F,C](s: LgHisto[C], i: int, offset: F=0.5): F =
  ## Geometric mean of left&right edge log-shifted `offset` fraction into bin
  if   i < s.n: -exp(s.aLn + s.h*(F(s.n - i - 1) + F(1) - offset))
  elif i > s.n: +exp(s.aLn + s.h*(F(i - s.n - 1) + offset))
  else: 0.0 # The bin containing x=zero cannot really be offset in the same way

func binAB*[F,C](s: LgHisto[C], x: F): (float, float) =
  ## Range in data space of the bin containing `x`; Costs 2 `fromIx`s.
  let i = s.toIx(x)
  if   i == 0      : result[0] = -Inf            ; result[1] = -s.b
  elif i == 1      : result[0] = -s.b            ; result[1] = s.fromIx(i, 1.0)
  elif i == 2*s.n-1: result[0] = s.fromIx(i, 0.0); result[1] = +s.b
  elif i == 2*s.n  : result[0] = +s.b            ; result[1] = +Inf
  elif x <  -s.a   : result[0] = s.fromIx(i, 0.0); result[1] = s.fromIx(i, 1.0)
  elif x >= +s.a   : result[0] = s.fromIx(i, 0.0); result[1] = s.fromIx(i, 1.0)
  else             : result[0] = -s.a            ; result[1] = +s.a

func add*[F,C](s: var LgHisto[C], x: F, w=1.C) =
  ## Increment bin for value `x` by weight `w`
  if not isNaN(x): s.bist.inc(s.toIx(x), +int(w))

func pop*[F,C](s: var LgHisto[C], x: F, w=1.C) =
  ## Alias for `add` with a negative weight argument
  if not isNaN(x): s.bist.inc(s.toIx(x), -int(w))

iterator bins*[C](s: LgHisto[C]): (float, float, C) =
  ## Yield `(lo, hi, count)` for each bin covered
  yield (-Inf, -s.b, s.bist.pmf 0)
  yield (-s.b, s.fromIx(1,1.0), s.bist.pmf 1)
  for i in  2   ..<  s.n: yield (s.fromIx(i,0.0),s.fromIx(i,1.0),s.bist.pmf i)
  yield (-s.a, s.a, s.bist.pmf s.n) # middle bin breaks geometric mean formula
  for i in s.n+1..<2*s.n-1: yield (s.fromIx(i,0.0),s.fromIx(i,1.0),s.bist.pmf i)
  yield (s.fromIx(2*s.n-1,0.0), +s.b, s.bist.pmf 1)
  yield (+s.b, +Inf, s.bist.pmf 2*s.n)

proc `$`*[C](s: LgHisto[C], nonZero=true): string =
  ## Formatting operator; Warning: output can be large, esp. if nonZero=false
  result.add "n: "   & $s.n   & "\ta: " & $s.a & "\tb: "    & $s.b    & "\n"
  result.add "aLn: " & $s.aLn & "\th: " & $s.h & "\thInv: " & $s.hInv & "\n"
  result.add "bins,cnts:\n"
  var tot = 0; var n = 0
  for (a, b, c) in s.bins:
    let c = int(c); tot += c
    if nonZero:
      if c != 0: result.add "  [ " & $a & " , " & $b & " ): " & $c & "\n"; inc n
    else       : result.add "  [ " & $a & " , " & $b & " ): " & $c & "\n"
  result[^1] = '\n'
  result.add "totalCount: " & $tot & " nonZeroBins: " & $n

func quantile*[F,C](s: LgHisto[C], q: F): F =
  ## Basic quantile; XXX Can log-spacing savvy interpolation be more accurate?
  if q < 0.0 or q > 1.0: return NaN
  var iL, iH: int
  let fL = s.bist.quantile(q, iL, iH)
  fL*s.fromIx(iL) + (1 - fL)*s.fromIx(iH)

func cdf*[F,C](s: LgHisto[C], x: F): C =
  ## Raw count; Leave to caller to multiply by 1/s.bist.count; XXX Interpolate?
  if x.isNaN: NaN else: s.bist.cdf(s.toIx(x))

func merge*[C](dst: var LgHisto[C], src: LgHisto[C]) =
  ## Merge counts from src into dst.
  if src.n != dst.n or src.a != dst.a or src.b != dst.b:
    raise newException(ValueError, "src-dst histogram parameter mismatch")
  for i in 0..2*src.n: dst.bist.inc i, src.bist.pmf(i) # Flat array can be fastr

when isMainModule:
  when defined(test): # Helpful to run against: \ -12 \ -8 \ -4 \ -1 0 1 4 8 12
    proc lghist(a=0.125, b=10.0, n=8, qs = @[0.25, 0.5, 0.75], xs: seq[float]) =
      var lh = initLgHisto[uint16](a, b, n)
      for x in xs: lh.add x
      echo `$`(lh, nonZero=false)
      for (a, b, c) in lh.bins:
        if (a,b) != lh.binAB((a+b)/2) or a >= b:
          echo "a: ",a," b: ",b," c: ",c," ab(mid(a,b)): ",lh.binAB((a+b)/2)
      for q in qs: echo "q",q,": ",lh.quantile(q)
    import cligen; dispatch lghist
  else:
    import std/[random, times]; randomize()
    var data: seq[float32]
    var res = newSeqOfCap[float32](9)
    const N = 750_000
#   for i in 1 .. N: data.add rand(0.0 .. 1.0)
    for i in 1 .. N: data.add gauss().float32
    var s = initLgHisto[uint32]()
    let t0 = epochTime()
    for x in data: s.add x
    let t1 = epochTime()
    for q in [0.001, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 0.999]:
      res.add s.quantile(q)
    let t2 = epochTime()
    for r in res: echo r  # do not time the formatting/echo part
    echo "ns/add: ", (t1-t0)*1e9/N.float, " ns/9qs: ", (t2-t1)*1e9
    echo "space: ", s.space, " bytes"
