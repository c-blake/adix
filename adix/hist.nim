##[`hist` provides simple regular histograms `Hist` and their cumulative form
`CHist` with a `Bist` interface.  They can be used with `adix/xhist1`.  In the
histogram aka differential mode, bin inc & dec is fast & `adix/cumsum` can be at
least tolerable via parallel prefix sum on x86_64.  Re-scaling|shifting bins can
also be done (externally) post-decrement & pre-increment and is vectorizable
which enables general weight decay, not only exponential/linear/flat, but adds
per-time-series point expense proportional to nBin.  For specific weight decays
this can be ~2X optimized some by "always counting up", as with Fenwick-backed
embist/lmbist, but since that comes with FP math trade offs we hold off for now.

Perf break-even vs Bist methods depends on (at least!) counter size/vectorizing,
platform, time kernel, and work load.  Ballpark expectations might be to use
`Hist` for <= ~300 bins for 1:1 ratios of dec, inc & quantile with this being
about 4X faster for 8 bins & nBin/lg(nBin) slower for larger nBin.  For `CHist`
and an equal operation mixture I saw Bist-break-even around ~405 bins.  `Hist`
can be strictly faster if your use case has many incs & decs per quantile query.
Both can also potentially be accelerated by GPUs (especially when transforming
whole, already existing arrays rather than online/incremental transformation).
The bottom of this module has a small test/timing program against a Bist.  The
300 & 405 numbers came from a i7-1370P, but on older i7-6700k were 160 & 223.
`Bist`'s are only ever ~2X slower than unstructured binning while unstructured
can be dramatically (>100X slower) for many bins.  So, if you have scale doubts
or cannot control the number of bins, `Bist`'s are much safer. ]##

when not declared assert: import std/assertions
import adix/cumsum, std/algorithm

type CHist*[N: SomeNumber] = object ## Simple Cumulative Histogram
  sum*: seq[N] ## CDF counter array

proc len*[N](c: CHist[N]): int = c.sum.len ## Number of bins & bytes
func size*[N](c: CHist[N]): int = c.len*N.sizeof
func space*[N](c: CHist[N]): int = c.sizeof + c.size
proc tot*[N](c: CHist[N]): N = c.sum[^1] ## Raw total
proc count*[N](c: CHist[N]): N = c.tot ## Total Weight
proc init*[N](c: var CHist[N]; len: int) = c.sum.setLen len
proc initCHist*[N](len: int): CHist[N] = result.init len
proc clear*[N](c: var CHist[N]) = zeroMem c.sum[0].addr, c.size
proc inc*[N](c: var CHist[N]; i: int, w: N=1) = ## += weight `w` to bins `>=i`
  for j in i..c.sum.high: c.sum[j] += w #XXX Can gcc auto-vectorize?
proc dec*[N](c: var CHist[N]; i: int; w: N=1) = ## -= weight `w` from bins `>=i`
  for j in i..c.sum.high: c.sum[j] -= w #XXX Can gcc auto-vectorize?

proc invCDF*[N](c: CHist[N], s: N; s0: var N): int =
  ## For `0 < s <= tot`, bracket ECDF jump `>= s`.  I.e. find `i0, s0` so `s0 =
  ## sum(..< i0) < s yet sum(..i0) >= s` in `lgCeil n` array probes.
  assert 0<=s and s<=c.tot,"CHist.invCDF OORange sum " & $s & " of " & $c.tot
  result = c.sum.lowerBound(s)         #NOTE: s<0|s>tot are invalid inputs
  if result >= c.sum.high: result = c.sum.high; s0 = c.tot
  else: s0 = (if result > 0: c.sum[result - 1] else: 0)

proc invCDF*[N](c: CHist[N], s: N): (int, N) = result[0] = c.invCDF(s,result[1])
  ## For `0 < s <= tot` return `(i0,s0)` so `sum(..<i0)=s0 < s and sum(..i0)>=s`

proc invCDF*[N](c: CHist[N]; s: N; s0, s1: var N): int =
  ## For `0 < s <= tot`, find `i0,s0,s1` so `s0 < s <= s1` and `s0+pmf(i0)==s1`.
  result = c.invCDF(s, s0)
  if result == c.sum.high: s1 = s0; s0 = c.sum[result - 1]
  else: s1 = c.sum[result]

proc min*[N](c: CHist[N]): int = ## Simple wrapper: invCDF(c, 1)
  var s0: N; c.invCDF(1, s0)

proc max*[N](c: CHist[N]): int = ## Simple wrapper: invCDF(c, c.count).
  var s0: N; c.invCDF(c.tot.N, s0)

from std/fenv import epsilon #XXX Centralize thrice replicated Parzen(invCDF)
proc quantile*[N](c: CHist[N]; q: float; iL,iH: var int): float =
  ## Parzen-interpolated quantile; E.g., q=0.9 => 90th percentile.  ``answer =
  ## result*iL + (1-result)*iH``, but is left to caller to do { in case it is
  ## mapping larger numeric ranges to/from iL,iH }.  Tm ~ ``2*lg(addrSpace)``.
  ## Unlike other (broken!) quantile-interpolation methods, Parzen's connects
  ## midpoints of vertical CDF jumps, not horizontal.  This makes more sense,
  ## corresponding to Wilcoxon 1945 & later tie mid-ranking recommendations.
  assert c.tot > 0, "quantile(Hist[N]) requires non-empty Hist."
  var sL0, sL1, sH0, sH1: N                 #You probably want to draw a CDF to
  let n  = c.tot.float                      #..fully understand this code.
  let qN = q*n
  let wq = when N is SomeFloat: N.epsilon*n else: 1.N # A Quantum Of Ctr Wgt
  if qN <= 0.5*wq.float    : iL = c.min;iH=0;return 1 #Early tails rets; Pure iL
  if qN >= n - 0.5*wq.float: iL = c.max;iH=0;return 1 #{Early body are pure iH.}
  let dqN=when N is SomeFloat: wq else: 1.5 # Min round-off + max odds high side
  iH = c.invCDF(N(qN + dqN), sH0, sH1)      # sH0<qN<=sH1 BUT can be ABOVE|
  var sMidH = 0.5*float(sH0 + sH1)          #..BELOW sMidH.  So, test & move as
  if sMidH < qN:                            #..needed.  When it does fail..,
    if sH1 < c.tot:                         #..we want the next higher bin.
      iH    = c.invCDF(sH1 + wq, sH0, sH1)
      sMidH = 0.5*float(sH0 + sH1)
    else: return 0                          #..unless @HIGHEST already=>all iH
  if sH0 < wq: return 0                     #For qN this small, iH = iL = min.
  iL = c.invCDF(sH0, sL0, sL1)              #..Also, cannot call invCDF(0).
  when N is SomeFloat:                      # Should be impossible,but round-off
    if sL1 > sH0 + wq:                      #..makes it happen sometimes & when
      iL = c.invCDF(sH0 - wq, sL0, sL1)     #..it does, we want next lower bin.
  let sMidL = 0.5*float(sL0 + sL1)          #Mid-vertJump(nxtLwrBin) gives line
  min (sMidH - qN)/(sMidH - sMidL), 1.0     #Runs of N.eps-sized bins=>anomalies

proc quantile*[N](c: CHist[N], q: float): float =
  ## Parzen-interpolated quantile when no caller index mapping is needed
  var iL, iH: int
  let fL = c.quantile(q, iL, iH)
  fL*iL.float + (1 - fL)*iH.float

func cdf*[N](c: CHist[N], i: int): N = c.sum[i]
func pmf*[N](c: CHist[N], i: int): N = c.sum[i] - (if i > 0: c.sum[i-1] else: 0)

type Hist*[N: SomeNumber] = object ## Simple Histogram w/CHist field
  cnt*: seq[N] ## PDF/PMF/counter array
  c*: CHist[N] ## Its Cumulative Histogram
  tot*: N      ## `c.sum[^1]`, but always up to date
  dirty*: bool ## Flag indicating if `.c` may be out of date from `.cnt`

func len*[N](h: Hist[N]): int = h.cnt.len ## Number of bins & bytes
func size*[N](h: Hist[N]): int = h.len*N.sizeof
func space*[N](h: Hist[N]): int = h.c.space + h.sizeof + h.size
func tot*[N](h: Hist[N]): N = h.tot ## Raw total
func count*[N](h: Hist[N]): N = h.tot ## Total Weight
func init*[N](h: var Hist[N]; len: int) = h.c.init len; h.cnt.setLen len
func initHist*[N](len: int): Hist[N] = result.init len
func clear*[N](h: var Hist[N]) =
  h.c.clear; zeroMem h.cnt[0].addr, h.size; h.tot = 0.0; h.dirty = false

func inc*[N](h: var Hist[N]; i:int, w:N=1) = h.cnt[i]+=w; h.tot+=w; h.dirty=true
  ## Add weight `w` to bin `i` & `.tot`; set dirty
func dec*[N](h: var Hist[N]; i:int; w:N=1) = h.cnt[i]-=w; h.tot-=w; h.dirty=true
  ## Subtract weight `w` from bin `i` & `.tot`; set dirty
func up*[N](h: ptr Hist[N]) = ## Update `.c.sum` field after various inc/dec's
  if h.dirty and h.c.sum.len > 0 and h.cnt.len > 0:
    copyMem h.c.sum[0].addr, h.cnt[0].addr, h[].size
    cumsum.cumsum h.c.sum; h.dirty = false
func invCDF*[N](h: Hist[N], s: N; s0: var N): int = ## Wrap `h.c` WITH `h.up`
  h.addr.up; h.c.invCDF s, s0
func cdf*[N](h: Hist[N], i: int): N = h.addr.up; h.c.cdf(i)
func pmf*[N](h: Hist[N], i: int): N = h.cnt[i]

func invCDF*[N](h: Hist[N], s: N): (int, N) = result[0] = h.invCDF(s, result[1])
func invCDF*[N](h: Hist[N]; s: N; s0, s1: var N): int = h.c.invCDF(s, s0, s1)
func min*[N](h: Hist[N]): int = h.c.min
func max*[N](h: Hist[N]): int = h.c.max
func quantile*[N](h: Hist[N]; q: float; iL,iH: var int): float =
  h.addr.up; h.c.quantile(q,iL,iH)
func quantile*[N](h: Hist[N], q: float): float = h.addr.up; h.c.quantile(q)

func nPDF*[H: Hist|CHist](h: H): seq[float32] =
  result.setLen h.len; let s = 1/h.tot.float32
  for i, r in mpairs result: r = s*h.pmf(i).float32
func nCDF*[H: Hist|CHist](h: H): seq[float32] =
  result.setLen h.len; let s = 1/h.tot.float32
  for i, r in mpairs result: r = s*h.cdf(i).float32
func `$`*[H: Hist|CHist](h: H): string = "tot: " & $h.count & " pmf: " & $h.nPDF

when isMainModule:
  const fast {.booldefine.} = false     # VERY limited differences below
  const cumu {.booldefine.} = false
  when not declared addFloat: import std/[syncio, formatFloat]
  import std/[times, strformat], cligen, cligen/sysUt
  when fast: import adix/bist
  proc hist(xs: seq[int], win=3, q = -2.0, pdf=false,cdf=false,time=false,
            xMn=0,xMx=7) =
    template toI(x): untyped = max(xMn, min(xMx, x)) - xMn   # Clip & shift
    if win < 2: Value !! "win " & $win & " too small"
    when fast: (var d = initBist[uint32](xMx - xMn + 1))
    elif cumu: (var d = initCHist[uint32](xMx - xMn + 1))
    else     : (var d = initHist[uint32](xMx - xMn + 1))
    let t0 = epochTime()
    var tQ = 0.0                # Report avg qtl to ensure compiler cannot elide
    for t, x in xs:
      let x = x.toI                     # xOld frm Deque=moreGeneral
      if t >= win: d.dec xs[t - win].toI, 1 # Remove leaving
      d.inc x, 1                            # Add entering
      if pdf: echo t," b: tot: ",d.tot," mPMF: ",d.nPDF
      if cdf: echo t," b: tot: ",d.tot," mCDF: ",d.nCDF
      if q > -2.0:
        if time: tQ += d.quantile(q)    # `formatFloat` slow=>just total
        else: echo d.quantile(q)        # Report inverseCDF(q)
    if time:
      let n = xs.len.float; let dt = (epochTime() - t0)*1e9/n
      stderr.write &"n: {xs.len} ns/no: {dt:.1f} w: {win} <mQ>: {tQ/n}\n"

  dispatch hist, short={"xMn":'a', "xMx":'b'}, help={"xs": "x values",
 "win" : "moving data window in points","q"  : "quantile to report; 0.5=median",
 "pdf" : "print PDF each time step"    ,"cdf": "print CDF each time step",
 "time": "print timing statistics",
 "xMn" : "`xs[i]` clipped to this `a` on `[a, xs]`",
 "xMx" : "`xs[i]` clipped to this `b` on `[xs, b]`"}
