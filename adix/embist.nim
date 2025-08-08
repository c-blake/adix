##[ This is like `bist.nim`, but *grows* weight added to counters (some kind of
floating point) exponentially as new updates happen.  Up to floating point
arithmetic, this is equivalent to `ctrs vector*= wOld; ctr[i] += 1`.  Doing it
this way allows BIST use for efficient exponentially weighted moving quantile.
It does need rescaling BUT this can be made rare.  For wOld=0.9, 1.11^(6736|842)
=~ 1.8e308|3.4e38 ~= dbl_max.  Re-scaling to tinies can ~2X that to ~13k|1.7k
data points.  Rescaling is very CPU-vectorizable & so should be 8..16x\*lg(nBin)
=~ 80..320X faster per it; <=~2X to total amortized cost at <136..270,000 bins.

Like EWMAs, IF YOU NEVER `dec` then this filter has technically infinite, yet
*usually* short time-memory, but can operate only with a small histogram space.
EWMoving Median inherits a usual high breakdown point/robustness in that, even
if infinite memory/having enduring time-breakdown, influencing the median takes
*MUCH* more than one wild data point long ago in history - it takes a downright
different epoch / non-stationarity which is not nearly as bothersome.

Also, note that weighting styles of distributions & averages are analogous but
distinct. `*=wOld,+=(1-wOld)*x` is a normalized average, but weight in *distros*
is about "faked-repetition" of relative weight.  So, while things like half-life
& lag are the same formulae, meaning varies since distros have MANY interacting
relative weights. (See `LMBist` for linear weight moving medians w/non-flat
recency weight + *strict* windows or `Bist` for flat, strict windows.)  The API
is the same as `Bist[F]`.  The bottom of this module has a small test/timing
program showing the differences.

Happy to cite someone, but as far as I can tell, this is a novel (though fairly
obvious) application of Fenwick BISTs for a fast EWMMedian transform.  Luxenberg
& Boyd (2024) "Exponentially Weighted Moving Models" does something ~100X more
complex & surely slower than the one-pass O(n\*lg nBin) (20 ns/item!) way done
here.  Coleman at https://randorithms.com/2022/01/31/streaming-histograms.html
has some nice animations, but it has pedagogical/poorly scaling O(n\*nBin) code.
It seems likely someone doing big data analytics has this somewhere, though and
I am happy to give credit when due.  Similarly, please cite this github repo if
this code inspires your work. ]##
when not declared assert: import std/assertions # debugging
import adix/bist, std/math, cligen/sysUt
template maxFinite(T: typedesc[SomeFloat]): T = # Should be in std/math, IMO
  when T is float32: 3.4028235e+38'f32
  elif T is float64 or T is float: 1.7976931348623157e+308'f64

type EMBist*[F: SomeFloat] = object ## Exponentially weighted moving distrib.
  cnt: Bist[F]     # Raw count;  This F *could* become its own generic param.
  w, grow: float64 # Running weight next data point will add, growth factor.

proc len*[F](d: EMBist[F]): int = d.cnt.data.len ## Number of bins & bytes
func space*[F](d: EMBist[F]): int = d.sizeof + d.cnt.space
proc tot*[F](d: EMBist[F]): F = d.cnt.tot ## Raw total
proc count*[F](d: EMBist[F]): F = d.tot   ## Total Weight

proc init*[F](d: var EMBist[F]; len: int, wOld: float=0.96875) =
  d.cnt.init len; d.w = 1.0; d.grow = F(1/wOld)  # start w at 1/thresh?
proc initEMBist*[F](len: int, wOld: float): EMBist[F] = result.init len, wOld
proc clear*[F](d: var EMBist[F]) = d.cnt.clear; d.tot = 0.0

proc inc*[F](d: var EMBist[F]; i: int, w: F=1) = ## Add weight `w` to bin `i`
  const lim = F.maxFinite/1e9   # 1e9 just to leave some room for `w` variation
  const scl = 1/lim
  # Can pair up *= scl (ensuring multiplier stays FP representable), but this is
  # ~pointless since it 2x's BOTH rarity AND cost.  Subtracting 2*lg lim from
  # binary exponents is pure rarity savings *IF* it can be vectorized similarly.
  d.cnt.inc i, w*d.w
  if d.cnt.tot > lim:           # Re-scale so future adds do not overflow
    for c in mitems d.cnt.data: c *= scl  # Both BIST counts..
    d.cnt.tot *= scl; d.w *= scl          #..and meta-data.
  d.w *= d.grow

proc scale*[F](d: EMBist[F]; age: int): F = 1/d.grow^age
  ## Scale for more rare un-count old; Can re-use if dec @same relative age.
proc dec*[F](d: var EMBist[F]; i: int; w, scale: F) = d.cnt.dec i, w*scale
  ## Un-count-old operation for more rare EW with strict windows

proc cdf*[F](d: EMBist[F], i: int): F = d.cnt.cdf(i) / d.count ## wrap Bist.cdf
proc pmf*[F](d: EMBist[F], i: int): F = d.cnt.pmf(i) / d.count ## wrap Bist.pdf
proc invCDF*[F](d: EMBist[F], s: F; s0: var F): int = d.cnt.invCDF s, s0
  ## wrap Bist.invCDF
proc invCDF*[F](d: EMBist[F]; s: F; s0,s1: var F): int = d.cnt.invCDF s, s0,s1
  ## wrap Bist.invCDF
proc min*[F](d: EMBist[F]): int = d.cnt.min ## Simple wrapper of `Bist.min`.
proc max*[F](d: EMBist[F]): int = d.cnt.max ## Simple wrapper of `Bist.max`.
proc quantile*[F](d: EMBist[F]; q: float; iL,iH: var int): float = ## wrap Bist.quantile
  d.cnt.quantile q, iL,iH
proc quantile*[F](d: EMBist[F]; q: float): float = d.cnt.quantile q ## wrap Bist.quantile

proc counts*[F](d: EMBist[F]): seq[F] =
  result.setLen d.cnt.len; for i, r in mpairs result: r = d.pmf(i).F

proc cumuls*[F](d: EMBist[F]): seq[F] =
  result.setLen d.cnt.len; for i, r in mpairs result: r = d.cdf(i).F

when isMainModule:
  type F = float64
  const slow {.booldefine.} = false     # VERY limited differences below
  when not declared addFloat: import std/[syncio, formatFloat]
  import std/[times, strformat], cligen
  proc embist(xs: seq[int], wOld=0.75, q = -2.0, pdf=false,cdf=false,time=false,
              xMn=0,xMx=7) =
    template toI(x): untyped = max(xMn, min(xMx, x)) - xMn   # Clip & shift
    if wOld <= 0: raise Value !! "wOld " & $wOld & " too small"
    if wOld >= 1: raise Value !! "wOld " & $wOld & " too big"
    when slow: (var d = initBist[F](xMx - xMn + 1))
    else     : (var d = initEMBist[F](xMx - xMn + 1, wOld))
    let t0 = epochTime()
    var tQ = 0.0            # Report avg qtl to ensure compiler cannot elide
    for t, x in xs:
      let x = x.toI                     # xOld frm Deque=moreGeneral
      when slow:                        # On full data win, decay ALL old weight
        for cnt in mitems d.data: cnt *= wOld # BIG LOOP
        d.tot *= wOld
        d.inc x, 1.0                    # Unit entering weight
      else:                             # Remove weight for leaving data point
        d.inc x, 1.0                    # Unit entering weight
      if pdf: echo t," b: tot: ",d.tot," ewmPMF: ",d.counts
      if cdf: echo t," b: tot: ",d.tot," ewmCDF: ",d.cumuls
      if q > -2.0:
        if time: tQ += d.quantile(q)    # `formatFloat` slow=>just total
        else: echo d.quantile(q)        # Report inverseCDF(q)
    if time:
      let n = xs.len.float; let dt = (epochTime() - t0)*1e9/n
      stderr.write &"n: {xs.len} ns/no: {dt:.1f} w: {wOld} <mQ>: {tQ/n}\n"

  dispatch embist, short={"xMn":'a', "xMx":'b'}, help={"xs": "x values",
 "wOld": "per-update decay factor" , "q"  : "quantile to report; 0.5=median",
 "pdf" : "print PDF each time step", "cdf": "print CDF each time step",
 "time": "print timing statistics",
 "xMn" : "`xs[i]` clipped to this `a` on `[a, xs]`",
 "xMx" : "`xs[i]` clipped to this `b` on `[xs, b]`"}
#[ A Zsh session showing basic correctness&boost of optimization.  Sets up env,
compiles ref & optimized; makes nums; Tests various q & w; Finally measures 'em.
  nim=(nim c -d:danger); t=/tmp/nums   # Set up
  $nim -d:slow -o=sembist embist; $nim embist
  ( for i in {1..10000}; printf " %s" $((RANDOM%8)) ) > $t
  ( for q in .1 .25 .5 .75 .9; { for x in {1..9}; { w=$[10./(10.+x)]
      paste <(./sembist -w$w -q.1 `<$t`) <(./embist -w$w -q.1 `<$t`) |
        awk '{print $1-$2}' | sort -g | tails -h1 -t1 }}) 2>/dev/null|unfold -n3
  ./sembist -b8300 -tw.99 -q.5 `<$t`; ./embist -b8300 -tw.99 -q.5 `<$t`
I get NO DIFF between ref & optimized, optimized about 112X faster at -b8300 and
always faster, even at -b2. ]#
