##[ `Bist[T]` (& clients `lghisto`, `mvstat`) already support quantiles over
moving data windows.  Sometimes one wants recent values to carry more weight in
summary statistics, as in a Linearly Weighted Moving Average.  In the context of
a distribution, one weights by replication - more copies of more recent points
vs. earlier & earlier.  While one can do this by literal data point repetition,
that expands both space & time costs.  So, one should prefer virtual replication
- a histogram putting weight into bins with the same time structure.

A naive implementation decays weight for each point in the window as each point
leaves and adds a new point with weight `w`.  This is `O(nBin)` - no faster than
a full rebuild.  This CAN be done with no loops longer than `lg(nBin)`, though.
The key insight is to "count up forever" adding in new points with weight `t+1`,
but subtract a virtual zero level.  If no actual duplicates exist, this 0-level
is simply `t-w`. Each duplicate @various lags (common w/binning, e.g. `lghisto`)
gets 1 "copy" of this virtual 0-level - which "stack up" in a bin.  This can be
handled with a *second* `Bist` tracking *only* membership.  The actual distro is
then the linear combination `cnt[] - zero*nLag[]`.

We thus get `LMBist[T]` which bundles up this dual `Bist[T]` idea.  The API is
the same as `Bist[T]` & `EMBist[T]`.  The bottom of this module has a small
test/timing prog showing the differences.

Happy to cite someone, but as far as I can tell, this is a completely novel
application of Fenwick BISTs for a fast Linear weight Moving Median filter
transform.  I certainly came up with it on my own.  The linearly weighted moving
median itself while mostly obvious from its term and the more famous LWMA is all
but unheard of in academic literature regardless of implementation efficiency.
Khodakarami, et. al 2019 about Parkinson's is literally *THE ONLY* match on
scholar.google.com as of this writing.  Cohen & Strauss were likely somehow
aware of the idea, but unaware of the "LWM Average" term when they decided to
name this "chordal weighting" in their Cohen,Strauss2003 SIGMOD paper.  Due to
its simplicity (see adix/embist) & especially terse *average* form, exponential
time kernels SO dominate that other things are usually unattended.  See, e.g.,
Akinshin 2023 "Weighted Quantile Estimators" which does *not* consider linear
time kernels (but is otherwise a very interesting paper).  It is true that one
needs to keep *just one window* of data points in a std/deque|ring buffer to
expire items, but the same is true of exponential weighting with strict windows,
needed for good history/"time breakdown point" behavior in either average or
quantile settings.  It is only fairly extreme scenarios where just one window
exceeds CPU L3 cache, let alone DRAM, though.  Please cite this github repo if
this code inspires your work. ]##
when not declared assert: import std/assertions # debugging
import adix/[bist, bitop], cligen/sysUt

type LMBist*[T: SomeNumber] = object
  cnt, nLag: Bist[T]    # Raw count, number of Lags in-window@`i`
  zero: T               # Window size, Bottom Level, Root Finding Guess/Return

proc len*[T](d: LMBist[T]): int = d.cnt.data.len
func space*[T](d: LMBist[T]): int = 2*(d.sizeof + (d.cnt.data.len + 1)*T.sizeof)
proc tot*[T](d: LMBist[T]): T = d.cnt.tot - d.zero*d.nLag.tot
proc count*[T](d: LMBist[T]): T = d.tot

proc init*[T](d: var LMBist[T]; len: int) = d.cnt.init len; d.nLag.init len
proc initLMBist*[T](len: int): LMBist[T] = result.init len
proc clear*[T](d: var LMBist[T]) = d.cnt.clear; d.nLag.clear; d.zero = 0

proc cdf*[T](d: LMBist[T], i: int): T = d.cnt.cdf(i) - d.zero*d.nLag.cdf(i)
proc pmf*[T](d: LMBist[T], i: int): T = d.cnt.pmf(i) - d.zero*d.nLag.pmf(i)

proc invCDF*[T](d: LMBist[T], s: T; s0: var T): int =
  var c = s                             #NOTE: s==0 | s > tot are invalid inputs
  cfor (var half = d.cnt.data.len.ceilPow2 shr 1), half != 0, half >>= 1:
    var m = result + half - 1           # midpoint in binary search
    if m < d.cnt.data.len and d.cnt[m] - d.zero*d.nLag[m] < c:
      c -= d.cnt[m] - d.zero*d.nLag[m]
      result = m + 1
  s0 = s - c

proc invCDF*[T](d: LMBist[T]; s: T; s0, s1: var T): int =
  result = d.invCDF(s, s0); s1 = s0 + d.pmf(result)
proc min*[T](d: LMBist[T]): int = d.nLag.min ## Simple wrapper of `d.nLag.min`.
proc max*[T](d: LMBist[T]): int = d.nLag.max ## Simple wrapper of `d.nLag.max`.

proc quantile*[T](d: LMBist[T]; q:float; iL,iH: var int): float=
  var sL0, sL1, sH0, sH1: T                 #You probably want to draw a CDF to
  let tot = d.tot; let n = tot.float        #..fully understand this code.
  let qN = q*n
  if qN <= 0.5    : iL = d.min; iH = 0; return 1 #Early rets for tails; Pure iL
  if qN >= n - 0.5: iL = d.max; iH = 0; return 1 #{Early for body are pure iH.}
  iH = d.invCDF(T(qN + 1.5), sH0, sH1)
  var sMidH = 0.5*float(sH0 + sH1)          #This guess works 90+% of the time..
  if sMidH < qN:                            #..but can fail for large sH1 - sH0.
    if sH1 < tot:                           #When it fails, want next higher bin
      iH    = d.invCDF(sH1 + 1, sH0, sH1)
      sMidH = 0.5*float(sH0 + sH1)
    else: return 0                          #..unless @HIGHEST already=>all iH
  if sH0 == 0: return 0                     #For qN this small, iH = iL = min.
  iL = d.invCDF(sH0, sL0, sL1)              #..Also, cannot call invCDF(0).
  let sMidL = 0.5*float(sL0 + sL1)          #Mid-vertJump(nxtLwrBin) gives line
  (sMidH - qN)/(sMidH - sMidL)

proc quantile*[T](d: LMBist[T]; q: float): float =
  var iL, iH: int
  let fL = d.quantile(q, iL, iH)
  fL*iL.float + (1 - fL)*iH.float

proc counts*[T](d: LMBist[T]): seq[float32] =
  result.setLen d.cnt.len; let s = 1/d.tot.float32
  for i, r in mpairs result: r = d.pmf(i).float32*s

proc cumuls*[T](d: LMBist[T]): seq[float32] =
  result.setLen d.cnt.len; let s = 1/d.tot.float32
  for i, r in mpairs result: r = d.cdf(i).float32*s

proc inc*[T](d: var LMBist[T]; i: int, w: T) =
  d.cnt.inc i, w; d.nLag.inc i, 1       # track both weight & membership

proc dec*[T](d: var LMBist[T]; i: int, w: T) =
  d.cnt.dec i, w; d.nLag.dec i, 1       # track both weight & membership
  d.zero += 1                           # & the bottom or virtual zero.

when isMainModule:
  const slow {.booldefine.} = false     # VERY limited differences below
  when not declared addFloat: import std/[syncio, formatFloat]
  import std/[times, strformat], cligen
  proc lmbist(xs: seq[int], win=3, q = -2.0, pdf=false,cdf=false,time=false,
              xMn=0,xMx=7) =
    template toI(x): untyped = max(xMn, min(xMx, x)) - xMn   # Clip & shift
    if win < 2: Value !! "win " & $win & " too small"
    when slow: (var d = initBist[uint32](xMx - xMn + 1))
    else     : (var d = initLMBist[uint32](xMx - xMn + 1))
    let t0 = epochTime()
    var tQ = 0.0            # Report avg qtl to ensure compiler cannot elide
    for t, x in xs:
      let x = x.toI                     # xOld frm Deque=moreGeneral
      when slow:                        # On full data win, decay ALL old weight
        if t >= win: (for tw in t - win ..< t: d.dec xs[tw].toI, 1) # BIG LOOP
        d.inc x, min(t + 1, win).uint32 # Small entering weight
      else:                             # Remove weight for leaving data point
        if t >= win: d.dec xs[t - win].toI, uint32(t + 1 - win)
        d.inc x, uint32(t + 1)          # Large entering weight
      if pdf: echo t," b: tot: ",d.tot," lwmPMF: ",d.counts
      if cdf: echo t," b: tot: ",d.tot," lwmCDF: ",d.cumuls
      if q > -2.0:
        if time: tQ += d.quantile(q)    # `formatFloat` slow=>just total
        else: echo d.quantile(q)        # Report inverseCDF(q)
    if time:
      let n = xs.len.float; let dt = (epochTime() - t0)*1e9/n
      stderr.write &"n: {xs.len} ns/no: {dt:.1f} w: {win} <mQ>: {tQ/n}\n"

  dispatch lmbist, short={"xMn":'a', "xMx":'b'}, help={"xs": "x values",
 "win" : "moving data window in points","q"  : "quantile to report; 0.5=median",
 "pdf" : "print PDF each time step"    ,"cdf": "print CDF each time step",
 "time": "print timing statistics",
 "xMn" : "`xs[i]` clipped to this `a` on `[a, xs]`",
 "xMx" : "`xs[i]` clipped to this `b` on `[xs, b]`"}
#[ A Zsh session showing basic correctness&boost of optimization.  Sets up env,
compiles ref & optimized; makes nums; Tests various q & w; Finally measures 'em.
  nim=(nim c -d:danger); t=/tmp/nums   # Set up
  $nim -d:slow -o=slmbist lmbist; $nim lmbist
  ( for i in {1..10000}; printf " %s" $((RANDOM%8)) ) > $t
  ( for q in .1 .25 .5 .75 .9; { for w in {2..10}; {
      paste <(./lmbist -w$w -q.1 `<$t`) <(./lmbist -w$w -q.1 `<$t`) |
        awk '{print $1-$2}' | sort -g | tails -h1 -t1 }}) 2>/dev/null|unfold -n3
  ./slmbist -tw756 -q.5 `<$t`; ./lmbist -tw756 -q.5 `<$t`
I get NO DIFF between ref & optimized, optimized about 25X faster.  I also get a
-w BreakEven of -w4 for when version marked "slow" is faster, but it's only 1.2X
faster at smallest sensical -w2.  So, -w2..4 not really worth conditioning. ]#
