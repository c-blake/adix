## `LgHisto` is a simple application of BISTs to log-spaced histo bins that can
## yield fast, tunable accuracy Parzen-interpolated quantiles with ~5X lower
## time cost & ~10X higher space cost than typical t-Digests.  Similar "ease of
## use" features (not costing a lot for high dynamic ranges) obtain.
##
## Worst case time of *quantile queries* is ~2..4x one `add` and MUCH more than
## 5X better than t-Digest making actual speed-up sensitive to operation mixes.
## { This may change, but present t-Digest impls, even tree ones, do a linear
## scan for quantile/CDFs.  None even offer "batch" APIs to do N quantiles in
## one such scan - an easy extension with O(nQtl) speed-up.  "Histo B-trees"
## should allow better scaling for such. }
## 
## Quantile accuracy is absolute {not relative to `q*(1-q)`} & easily estimated
## as +-1/2 bin width which is about 10^(lg(b/a)/n/2).  So, if you need 3 places
## or your data is clustered very tightly within some given order of magnitude
## then you can probably just use 1e5 bins and still be very L1 cache resident
## on the "hot" part of your data depending upon competition.  Cache is the main
## cost Re: speed.  Since many bins are 0 in many settings, run-length encoding
## is likely enough for net/disk transfers of low entropy distros.
##
## BISTs also differ from t-Digests in other important ways. First, BISTs are
## well suited for `pop` or moving data window operations with *strict* finite
## memory, for e.g. translation of full streams to moving quantiles as in a
## Bollinger Band style "smooth".  Second, floating point weights for EWMA-like
## decaying memory are not possible since FP arithmetic kind of breaks BISTs.
##
## The way Fenwick BISTs work, the generic parameter `C` must be a wide enough
## integer type to hold both elemental bin counts and grand totals.  uint32 is
## likely enough for many applications, though some might sneak by with uint16
## and a few might need uint64.  This scales bin size/space cost.

import adix/bist, math
type
  LgHisto*[C] = object  ## Log-spaced histogram Bist[C] backing
    und, ovr: int       # Out of bounds under & over; For all *ever* `add`ed.
    n: int              # number of bins
    a, b: float         # histogram covers [-b, -a], (-a, a) in zero, [a, b]
    aLn, h, hInv: float # index scale conversion pre-computes
    bist: Bist[C]       # actual smart array of counters

func underflows*[C](s: LgHisto[C]): int = s.und
func overflows*[C](s: LgHisto[C]): int  = s.ovr
func low*[C](s: LgHisto[C]): float      = s.a
func high*[C](s: LgHisto[C]): float     = s.b
func nBin*[C](s: LgHisto[C]): int       = s.n
func bist*[C](s: LgHisto[C]): Bist[C]   = s.bist

func init*[C](s:var LgHisto[C], a=1e-16, b=1e20, n=8300) =
  ## Initialized a log-spaced histogram.
  if b <= a: raise newException(ValueError,"inverted: [" & $a & "," & $b & "]")
  if a <= 0.0 or b <= 0.0: raise newException(ValueError,"a,b must both be > 0")
  s.n    = n
  s.a    = a
  s.b    = b
  s.aLn  = ln(a)
  s.h    = (ln(b) - ln(a))/n.float
  s.hInv = 1.0/s.h
  s.bist = initBist[C](2*n + 1)

func initLgHisto*[C](a=1e-16, b=1e20, n=8300): LgHisto[C] = result.init a,b,n
  ## Return an initialized log-spaced histogram.

func space*[C](s: LgHisto[C]): int = s.sizeof + s.bist.space
  ## Estimate space taken up by data structure in bytes

func toIx*[F,C](s: var LgHisto[C], x: F): int =
  ## Find bin index for value `x`
  if   x <= -s.a:
    if x >= -s.b: result = s.n - 1 - int( (ln(-x) - s.aLn)*s.hInv)
    else        : s.und.inc; result = 0
  elif x >= +s.a:
    if x <= +s.b: result = s.n + 1 + int( (ln(+x) - s.aLn)*s.hInv)
    else        : s.ovr.inc; result = 2*s.n
  else: result = s.n

func fromIx*[F,C](s: LgHisto[C], i: int, offset: F=0.5): F =
  ## Geometric mean of left & right edge is ~2X faster than arithmetic mean
  if   i < s.n: -exp(s.aLn + s.h*(F(s.n - i) + offset))
  elif i > s.n: +exp(s.aLn + s.h*(F(i - s.n - 1) + offset))
  else: 0.0

func add*[F,C](s: var LgHisto[C], x: F, w=1.C) =
  ## Increment bin for value `x` by weight `w`
  if not isNaN(x): s.bist.inc(s.toIx(x), +int(w))

func pop*[F,C](s: var LgHisto[C], x: F, w=1.C) =
  ## Alias for `add` with a negative weight argument
  if not isnan(x): s.bist.inc(s.toIx(x), -int(w))

iterator bins*[C](s: LgHisto[C]): (float, float, C) =
  ## Yield `(lo, hi, count)` for each bin covered
  for i in  0  ..<   s.n : yield (s.fromIx(i,0.0),s.fromIx(i,-1.0),s.bist.pmf i)
  yield (-s.a, s.a, s.bist.pmf s.n)
  for i in s.n+1 .. 2*s.n: yield (s.fromIx(i,0.0),s.fromIx(i,+1.0),s.bist.pmf i)

proc `$`*[C](s: LgHisto[C], nonZero=true): string =
  ## Formatting operator; Warning: output can be large, esp. if nonZero=false
  result.add "n: "   & $s.n   & "\ta: "   & $s.a   & "\tb: "    & $s.b    & "\n"
  result.add "aLn: " & $s.aLn & "\th: "   & $s.h   & "\thInv: " & $s.hInv & "\n"
  result.add "und: " & $s.und & "\tovr: " & $s.ovr & "\tbins,cnts:\n"
  var tot = 0; var n = 0
  for (a, b, c) in s.bins:
    tot += int(c)
    if nonZero:
      if c != 0: result.add "  [ " & $a & " , " & $b & " ]: " & $c & "\n"; inc n
    else       : result.add "  [ " & $a & " , " & $b & " ]: " & $c & "\n"
  result[^1] = '\n'
  result.add "totalCount: " & $tot & " inNon0bins: " & $n

func quantile*[F,C](s: LgHisto[C], q: F): F =
  ## Basic quantile; XXX Can log-spacing savvy interpolation be more accurate?
  if q < 0.0 or q > 1.0: return NaN
  var iL, iH: int
  let fL = s.bist.quantile(q, iL, iH)
  fL*s.fromIx(iL) + (1 - fL)*s.fromIx(iH)

func cdf*[F,C](s: var LgHisto[C], x: F): C =
  ## Raw count; Leave to caller to multiply by 1/s.bist.count; XXX Interpolate?
  if x.isnan: NaN else: s.bist.cdf(s.toIx(x))

when isMainModule:
  when defined(test):# Helpful to run against: -12 -8 -4 -1 0 1 4 8 12
    proc lghist(a=0.125, b=10.0, n=8, qs = @[0.50], nums: seq[float]) =
      var lh = initLgHisto[uint16](a, b, n)
      for x in nums: lh.add x
      echo "lh: ", lh
      for q in qs: echo "q", q, " ", lh.quantile(q)
    import cligen; dispatch lghist
  else:
    import random, times; randomize()
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
