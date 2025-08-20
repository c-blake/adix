##[ This provides a simple regular histogram with the `bist.nim` interface, but
using vanilla bin counters mostly so it can be used with adix/xhist1.  Bin incs
are fast & `adix/cumsum` can be at least tolerable via parallel prefix sum on
x86_64.  Re-scaling|shifting bins can also be done (externally) pre-increment
and is eminently vectorizable.  This enables general time weight kernels, not
only exponential/linear/flat.  Performance break-even vs BIST methods depends on
(at least!) counter size / vectorization, platform, time kernel, and work load.
Ballpark expectations might be to use this <= ~300 bins for 1:1 ratios of dec,
inc & quantile with this being about 4X faster for 8 bins & nBin/lg(nBin) slower
for much larger nBin.  If your use case has many counter updates per quantile
query this could be strictly faster.  This can also potentially be accelerated
by GPUs (especially in the context of transforming whole, already existing
arrays rather than online / incremental transformation).  The bottom of this
module has a small test/timing program against a bist.

For specific time weight kernels this could be ~2X optimized a little by "always
counting up" as with Fenwick-backed embist/lmbist, but we hold off on that for
now to provide a distinct calculation with distinct FP math trade offs. ]##
when not declared assert: import std/assertions
import adix/cumsum, std/algorithm

type Hist*[N: SomeNumber] = object ## Simple Histogram
  cnt*, csum*: seq[N] ## PDF/PMF/counter array and its cumulative sum
  tot*: N             ## csum[^1], but always up to date
  dirty*: bool        ## Flag indicating if .csum may be out of date from .cnt

proc len*[N](h: Hist[N]): int = h.cnt.len ## Number of bins & bytes
func size*[N](h: Hist[N]): int = h.len*N.sizeof
func space*[N](h: Hist[N]): int = h.sizeof + 2*h.size
proc tot*[N](h: Hist[N]): N = h.tot ## Raw total
proc count*[N](h: Hist[N]): N = h.tot ## Total Weight

proc init*[N](h: var Hist[N]; len: int) = h.cnt.setLen len; h.csum.setLen len
proc initHist*[N](len: int): Hist[N] = result.init len
proc clear*[N](h: var Hist[N]) =
  zeroMem h.cnt[0].addr, h.size; zeroMem h.csum[0].addr, h.size
  h.tot = 0.0; h.dirty = false
proc inc*[N](h: var Hist[N]; i:int, w:N=1) = h.cnt[i]+=w; h.tot+=w; h.dirty=true
  ## Add weight `w` to bin `i` & `.tot`; set dirty
proc dec*[N](h: var Hist[N]; i:int; w:N=1) = h.cnt[i]-=w; h.tot-=w; h.dirty=true
  ## Subtract weight `w` from bin `i` & `.tot`; set dirty
proc up*[N](h: var Hist[N]) = ## Update `.csum` field after various inc/dec's
  if h.dirty and h.csum.len > 0 and h.cnt.len > 0:
    copyMem h.csum[0].addr, h.cnt[0].addr, h.size
    cumsum.cumsum h.csum; h.dirty = false

proc cdf*[N](h: Hist[N], i: int): N = h.csum[i]
proc pmf*[N](h: Hist[N], i: int): N = h.cnt[i]

proc invCDF*[N](h: Hist[N], s: N; s0: var N): int =
  ## For `0 < s <= tot`, bracket ECDF jump `>= s`.  I.e. find `i0, s0` so `s0 =
  ## sum(..< i0) < s yet sum(..i0) >= s` in `lgCeil n` array probes.
  assert 0<=s and s<=h.tot, "Hist.invCDF OORange sum " & $s & " of " & $h.tot
  result = h.csum.lowerBound(s)         #NOTE: s<0|s>tot are invalid inputs
  if result == h.cnt.high: s0 = h.csum[result]
  else: s0 = h.csum[result] - h.cnt[result]

proc `$`*[N](h: Hist[N]): string = "tot: " & $h.count & " pmf: " & $h.nPDF

proc invCDF*[N](h: Hist[N], s: N): (int, N) = result[0] = h.invCDF(s, result[1])
  ## For `0 < s <= tot` return `(i0,s0)` so `sum(..<i0)=s0 < s and sum(..i0)>=s`

proc invCDF*[N](h: Hist[N]; s: N; s0, s1: var N): int =
  ## For `0 < s <= tot`, find `i0,s0,s1` so `s0 < s <= s1` and `s0+pmf(i0)==s1`.
  result = h.invCDF(s, s0)
  if result == h.cnt.high: s1 = s0; s0 = s1 - h.cnt[result]
  else: s1 = s0 + h.cnt[result]

proc min*[N](h: Hist[N]): int = ## Simple wrapper: invCDF(h, 1)
  var s0: N; h.invCDF(1, s0)

proc max*[N](h: Hist[N]): int = ## Simple wrapper: invCDF(h,h.count).
  var s0: N; h.invCDF(h.tot.N, s0)

from std/fenv import epsilon #XXX Centralize thrice replicated Parzen(invCDF)
proc quantile*[N](h: Hist[N]; q: float; iL,iH: var int): float =
  ## Parzen-interpolated quantile; E.g., q=0.9 => 90th percentile.  ``answer =
  ## result*iL + (1-result)*iH``, but is left to caller to do { in case it is
  ## mapping larger numeric ranges to/from iL,iH }.  Tm ~ ``2*lg(addrSpace)``.
  ## Unlike other (broken!) quantile-interpolation methods, Parzen's connects
  ## midpoints of vertical CDF jumps, not horizontal.  This makes more sense,
  ## corresponding to Wilcoxon 1945 & later tie mid-ranking recommendations.
  assert h.tot > 0, "quantile(Hist[N]) requires non-empty Hist."
  var sL0, sL1, sH0, sH1: N                 #You probably want to draw a CDF to
  let n  = h.tot.float                      #..fully understand this code.
  let qN = q*n
  let wq = when N is SomeFloat: N.epsilon*n else: 1.N # A Quantum Of Ctr Wgt
  if qN <= 0.5*wq.float    : iL = h.min;iH=0;return 1 #Early tails rets; Pure iL
  if qN >= n - 0.5*wq.float: iL = h.max;iH=0;return 1 #{Early body are pure iH.}
  let dqN=when N is SomeFloat: wq else: 1.5 # Min round-off + max odds high side
  iH = h.invCDF(N(qN + dqN), sH0, sH1)      # sH0<qN<=sH1 BUT can be ABOVE|
  var sMidH = 0.5*float(sH0 + sH1)          #..BELOW sMidH.  So, test & move as
  if sMidH < qN:                            #..needed.  When it does fail..,
    if sH1 < h.tot:                         #..we want the next higher bin.
      iH    = h.invCDF(sH1 + wq, sH0, sH1)
      sMidH = 0.5*float(sH0 + sH1)
    else: return 0                          #..unless @HIGHEST already=>all iH
  if sH0 < wq: return 0                     #For qN this small, iH = iL = min.
  iL = h.invCDF(sH0, sL0, sL1)              #..Also, cannot call invCDF(0).
  when N is SomeFloat:                      # Should be impossible,but round-off
    if sL1 > sH0 + wq:                      #..makes it happen sometimes & when
      iL = h.invCDF(sH0 - wq, sL0, sL1)     #..it does, we want next lower bin.
  let sMidL = 0.5*float(sL0 + sL1)          #Mid-vertJump(nxtLwrBin) gives line
  min (sMidH - qN)/(sMidH - sMidL), 1.0     #Runs of N.eps-sized bins=>anomalies

proc quantile*[N](h: Hist[N], q: float): float =
  ## Parzen-interpolated quantile when no caller index mapping is needed
  var iL, iH: int
  let fL = h.quantile(q, iL, iH)
  fL*iL.float + (1 - fL)*iH.float

proc nPDF*[N](h: Hist[N]): seq[float32] =
  result.setLen h.len;let s=1/h.tot.float32;for i,r in mpairs result:r=s*h.pmf(i).float32

proc nCDF*[N](h: Hist[N]): seq[float32] =
  result.setLen h.len;let s=1/h.tot.float32;for i,r in mpairs result:r=s*h.cdf(i).float32

when isMainModule:
  const fast {.booldefine.} = false     # VERY limited differences below
  when not declared addFloat: import std/[syncio, formatFloat]
  import std/[times, strformat], cligen, cligen/sysUt
  when fast: import adix/bist
  proc hist(xs: seq[int], win=3, q = -2.0, pdf=false,cdf=false,time=false,
            xMn=0,xMx=7) =
    template toI(x): untyped = max(xMn, min(xMx, x)) - xMn   # Clip & shift
    if win < 2: Value !! "win " & $win & " too small"
    when fast: (var d = initBist[uint32](xMx - xMn + 1))
    else     : (var d = initHist[uint32](xMx - xMn + 1))
    let t0 = epochTime()
    var tQ = 0.0                # Report avg qtl to ensure compiler cannot elide
    for t, x in xs:
      let x = x.toI                     # xOld frm Deque=moreGeneral
      if t >= win: d.dec xs[t - win].toI, 1 # Remove leaving
      d.inc x, 1                            # Add entering
      if pdf: echo t," b: tot: ",d.tot," mPMF: ",d.nPDF
      when not fast: d.up       # Make callers do this only once @top-level
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
