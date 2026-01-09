##[ `xhist1.def` is a template to make a histogram type for univariate/1D data
monotonically transformed over (0,+Inf) to bin via any integer-keyed backing
histo.  The backing histo is itself generic over time weighting kernels (flat,
linear, exponential) & those over their counter types (exponential time-decay
needs `SomeFloat`).  Finally, a `xhist1.defMove` template makes a wrapper type
of a histo type that enables easy "only add & quantile query" use.  An example
program at the bottom of the module should mostly show how to use these.

This generalizes `adix/lghisto`.  Quantile error is bounded by quantization
error which is <~1/2 transformed bin width - eg. ~10^(log10(b/a)/n) w/X=log.
Space-time trade-offs depend on how boundable data ranges are, but underflows &
overflows are at least counted (until counters saturate!).

Definitions made by templates are not marked for export - `xhist1.exportDefs` if
you want that.  Also, I am aware that dynamic dispatch could be useful here, but
that adds a noticeable overhead in tight loops over big data. ]##

when not declared addFloat: import std/formatfloat; export formatfloat
from std/math import isNaN; from std/fenv import epsilon; export isNaN, epsilon
import cligen/sysUt; export sysUt; import lna; export lna
import std/deques; export deques

template def*(T, X, X⁻¹, H; Hini: typed = false; Harg=0.0) =
  ##[ Here `T` is the type that will be defined along with an `init T`, `H` is
  the type of histogram (e.g. `Bist[uint32]`), `X` is an expression transforming
  monotonically over `(0, +Inf)` some input `x`, e.g. `ln`, while `X⁻¹` is its
  inverse function expression, e.g. `exp`.  So, `def Histo,ln,exp,Bist[uint32]`
  is one instantiation.  Besides defining the type, this also defines routines
  documented in detail over in https://c-blake.github.io/adix/adix/lghisto.html
   * func underflows(s: T): type(s.hist.cdf 0)      # bin[0]
   * func overflows(s: T) : type(s.hist.cdf 0)      # bin[^1]
   * func low(s: T): float                          # `.a`
   * func high(s: T): float                         # `.b`
   * func nBin(s: T): int                           # `.n` - 2*n+1 is num.bins
   * func hist(s: T): H                             # `.hist` - backing histo
   * proc init(s: var T, a=1e-16, b=1e20, n=8300)   # init w/2*n+1 bins
   * proc initT(a=1e-16, b=1e20, n=8300): T         # Same, but w/TypeName
   * func space(s: T): int                          # Est.of total bytes used
   * func tot(s: T): auto                           # Total count weight
   * func toIx[F](s: T, x: F): int                  # x -> bin index
   * func fromIx[F](s: T, i: int, offset: F=0.5): F # bin index -> bin center
   * func binAB[F](s: T, x: F): (float, float)      # whole range for bin(`x`)
   * func add[F](s: var T, x: F, w: type(s.hist.cdf 0) = 1)  # inc wt by w
   * func pop[F](s: var T, x: F, w: type(s.hist.cdf 0) = 1)  # dec wt by w
   * iterator bins(s: T): (float, float, type(s.hist.cdf 0)) # yield (lo,hi,cnt)
   * func `$`(s: T, nonZero=true): string                    # format histo
   * func cdf[F](s: T, x: F): type(s.hist.cdf 0)    # Raw count; Callers /s.tot
   * func quantile[F](s: T, q: F): F                # Basic quantile
   * func merge(dst: var T, src: T)                 # Cnts from src into dst ]##

  type `T` = object       ## histogram(X(x[])) with backing histogram `H`
    n*: int               ## number of bins
    a*, b*: float         ## histogram covers [-b, -a], (-a, a) in zero, [a, b]
    aX*, h*, hInv*: float ## pre-computes for index scale conversion
    hist*: H              ## actual smart array of counters: [0, 2*n] -> PMF/CDF
#XXX Take `noNegative` option; Untouched cache matters not, BUT per point Deque
# space keys off width of indices. 2X can help here. Eg, (10^(1/256))^11 =~ 1.1.

  func underflows(s: `T`): type(s.hist.cdf 0) = s.hist.pmf 0
  func overflows(s: `T`) : type(s.hist.cdf 0) = s.hist.pmf 2*s.n
  func low(s: `T`): float      = s.a
  func high(s: `T`): float     = s.b
  func nBin(s: `T`): int       = s.n
  func hist(s: `T`): H         = s.hist

  proc init(s: var `T`, a=1e-16, b=1e20, n=8300) =
    ## Init histo w/2n+1 X-spaced bins: `[-∞..-b; -b..-a; 0; a..<b; b..∞]`.
    if b <= a: Value !! "inverted: [" & $a & "," & $b & "]"
    if a <= 0.0 or b <= 0.0: Value !! "a,b must both be >0"
    if n < 2: Value !! "n must >= 2"
    s.n    = n
    s.a    = a
    s.b    = b
    s.aX   = a.X
    s.h    = (b.X - s.aX)/float(n - 1)
    s.hInv = 1.0/s.h
    when Hini: s.hist.init 2*n + 1, Harg
    else     : s.hist.init 2*n + 1

  proc `init T`(a=1e-16, b=1e20, n=8300): `T` = result.init a, b, n
    ## Get Histo w/2n+1 X-spaced bins: `[-inf..<-b; -b..<-a; 0; a..<b; b..inf]`.

  func space(s: `T`): int = s.sizeof + s.hist.space
    ## Estimate space taken up by data structure in bytes

  func tot(s: `T`): auto = s.hist.tot ## Give total count weight

  func toIx[F](s: `T`, x: F): int =
    ##Find bin index for value `x`; Underflows get `[0]`, Overflows get `[2*n]`.
    if   x <= -s.a:
      if x >= -s.b: result = s.n - 1 - int( (X(-x) - s.aX)*s.hInv)
      else        : result = 0
    elif x >= +s.a:
      if x <= +s.b: result = s.n + 1 + int( (X(+x) - s.aX)*s.hInv)
      else        : result = 2*s.n
    else: result = s.n

  func fromIx[F](s: `T`, i: int, offset: F=0.5): F =
    ## X⁻¹-mean of left&right edge X-shifted `offset` fraction into bin
    if   i < s.n: -X⁻¹(s.aX + s.h*(F(s.n - i - 1) + F(1) - offset))
    elif i > s.n: +X⁻¹(s.aX + s.h*(F(i - s.n - 1) + offset))
    else: 0.0 # Bin containing x=zero cannot really be offset in the same way

  func binAB[F](s: `T`, x: F): (float, float) =
    ## Range in data space of the bin containing `x`; Costs 2 `fromIx`s.
    let i = s.toIx(x)
    if   i == 0      : result[0] = -Inf           ; result[1] = -s.b
    elif i == 1      : result[0] = -s.b           ; result[1] = s.fromIx(i,1.0)
    elif i == 2*s.n-1: result[0] = s.fromIx(i,0.0); result[1] = +s.b
    elif i == 2*s.n  : result[0] = +s.b           ; result[1] = +Inf
    elif x <  -s.a   : result[0] = s.fromIx(i,0.0); result[1] = s.fromIx(i,1.0)
    elif x >= +s.a   : result[0] = s.fromIx(i,0.0); result[1] = s.fromIx(i,1.0)
    else             : result[0] = -s.a           ; result[1] = +s.a

  func add[F](s: var `T`, x: F, w: type(s.hist.cdf 0)=1): int {.discardable.} =
    ## Increment bin for value `x` by weight `w`             #..caller can save
    if not isNaN(x): result = s.toIx(x); s.hist.inc result, w
    else: result = int.low

  func pop[F](s: var `T`, x: F, w: type(s.hist.cdf 0) = 1) =
    ## Alias for `add` with a negative weight argument
    if not isNaN(x): s.hist.dec s.toIx(x), w

  func pop[F](s: var `T`, i: int, w: type(s.hist.cdf 0) = 1) =
    ## `pop` that takes index output of `add` as an argument, to skip `toIx`.
    if i != int.low: s.hist.dec i, w

  iterator bins(s: `T`): (float, float, type(s.hist.cdf 0)) =
   ## Yield `(lo, hi, count)` for each bin covered
   yield (-Inf, -s.b, s.hist.pmf 0)
   yield (-s.b, s.fromIx(1,1.0), s.hist.pmf 1)
   for i in  2   ..<  s.n  :yield (s.fromIx(i,0.0),s.fromIx(i,1.0),s.hist.pmf i)
   yield (-s.a, s.a, s.hist.pmf s.n) # middle bin breaks X⁻¹-mean formula
   for i in s.n+1..<2*s.n-1:yield (s.fromIx(i,0.0),s.fromIx(i,1.0),s.hist.pmf i)
   yield (s.fromIx(2*s.n-1,0.0), +s.b, s.hist.pmf 2*s.n-1)
   yield (+s.b, +Inf, s.hist.pmf 2*s.n)

  func `$`(s: `T`, minP=2.0*float32.epsilon): string =
    ## Formatting operator; Warning: output can be large, esp. if minP <= 0
    result.add "n: "  & $s.n  & "\ta: " & $s.a & "\tb: "    & $s.b    & "\n"
    result.add "aX: " & $s.aX & "\th: " & $s.h & "\thInv: " & $s.hInv & "\n"
    result.add "totalCount:" & $s.tot & " bins,cnts:\n"
    var n = 0; let t = s.tot.float*minP
    for (a, b, c) in s.bins:
      if c.float >= t:
        result.add "  [ " & $a & " , " & $b & " ): " & $c & "\n"; inc n
    result[^1] = '\n'
    result.add $n & " bins>=" & $t

  func cdf[F](s: `T`, x: F): type(s.hist.cdf 0) =
    ## Raw count; Leave to caller to multiply by 1/s.hist.count;XXX Interpolate?
    when type(s.hist.cdf 0) is SomeInteger: # ints HAVE NO NaN -> result.low
      if x.isNaN: low(type(result)) else: s.hist.cdf(s.toIx(x)) #!!! unsigned==0
    else:
      if x.isNaN: NaN else: s.hist.cdf(s.toIx(x))

  func quantile[F](s: `T`, q: F): F =
    ## Basic quantile; XXX More accurate X-spacing-savvy interpolation?
    if q < 0.0 or q > 1.0: return NaN
    var iL, iH: int
    let fL = s.hist.quantile(q, iL, iH)
    fL*s.fromIx(iL) + (1 - fL)*s.fromIx(iH)

  proc merge(dst: var `T`, src: `T`) =
    ## Merge counts from src into dst.
    if src.n != dst.n or src.a != dst.a or src.b != dst.b:
      Value !! "src-dst histogram parameter mismatch"
    for i in 0..2*src.n: dst.hist.inc i, src.hist.pmf(i) # Flat array prob fastr

template exportDefs*(T) =
  export `T`, underflows, overflows, low, high, nBin, hist, init, `init T`, tot,
    space, toIx, fromIx, binAB, add, pop, bins, `$`, cdf, quantile, merge

template defMove*(T, X, wEntering, wLeaving; nMx=32767) =
  type `T` = object ## Layer maybe-time-weighted moving win over transforming X
    ix*: Deque[when nMx<=127:uint8 elif nMx<=32767:uint16 else:uint32] ## index
    xwh*: X         ## Transformed, time-Weighted Histogram
    t*, win*: int   ## Current time index, window length

  proc `init T`(a=1e-16, b=1e20, n=8300, win=10): T =
    if n > nMx: Value !! "`n` too large; max is " & $nMx
    result.xwh = `init X`(a, b, n); result.win = win

  func add[F](m: var T, x: F) =
    template it: var `T` {.inject.} = m
    if m.t >= m.win:
      m.xwh.hist.dec m.ix.popFirst.int, type(m.xwh.hist.cdf 0)(wLeaving)
    let i = m.xwh.add(x, type(m.xwh.hist.cdf 0)(wEntering))
    m.ix.addLast (when nMx<=127:i.uint8 elif nMx<=32767:i.uint16 else: i.uint32)
    m.t.inc

  func cdf[F](m: `T`, x: F): type(m.xwh.hist.cdf 0) = m.xwh.cdf x

  func quantile[F](m: `T`, q: F): F = m.xwh.quantile q

  func space(m: `T`): int = m.sizeof + m.xwh.space + m.ix.len*m.ix[0].sizeof

  func `$`(m: `T`, minP=2.0*float32.epsilon): string = `$`(m.xwh, minP)

  iterator bins(m: `T`): (float, float, type(m.xwh.hist.cdf 0)) =
    for tup in m.xwh.bins: yield tup

  func binAB[F](m: `T`, x: F): (float, float) = m.xwh.binAB(x)

  func tot(m: `T`): auto = m.xwh.hist.tot

when isMainModule:
  {.push warning[UnusedImport]:off.}; {.push hint[DuplicateModuleImport]:off.}
  import adix/[hist, bist, embist, lmbist]  # Import everything we *might*..
  from std/math import exp, sqrt            #..need rather than condition.
  template Id(x): untyped  = x              # The identity/linear transform
  template sqr(x: untyped) = x*x            # The sqrt-sqr transform
  const mo {.intdefine.} = 0
  type U4 = uint32; type F4 = float32
  when mo==0:def His  ,Id ,Id ,Hist[U4]
  elif mo==1:def His  ,Id ,Id ,CHist[U4]
  elif mo==2:def His  ,Id ,Id ,Bist[U4]
  elif mo==3:def His  ,lna,exp,Bist[U4]  # Match LgHisto
  elif mo==4:def His ,sqrt,sqr,Bist[U4]
  elif mo==5:def His  ,lna,exp,EMBist[F4], Hini=true, 0.9375
  elif mo==6:def FHist,lna,exp,Hist[U4]  ;defMove His,FHist, 1, 1
  elif mo==7:def FCist,lna,exp,CHist[U4] ;defMove His,FCist, 1, 1
  elif mo==8:def FBist,lna,exp,Bist[U4]  ;defMove His,FBist, 1, 1
  elif mo==9:def LBist,lna,exp,LMBist[U4];defMove His,LBist,it.t+1,it.t+1-it.win
  elif mo==10:(def(EBist,lna,exp,EMBist[F4], Hini=true, 0.9375); # Need all ()s
                 defMove(His, EBist, 1.0, 1.0/it.xwh.hist.scale(it.win)))
  when defined test: # Helpful to run against: -- -12 -8 -4 -1 0 1 4 8 12 [ 8 ]
    proc lghist(a=0.125,b=10.0,n=8, win=8,qs = @[0.25,0.5,0.75], xs:seq[float])=
      when compiles(initHis(a, b, n, win)): (var lh = initHis(a, b, n, win))
      else: (var lh = initHis(a, b, n))
      for x in xs: lh.add x
      echo `$`(lh, minP=0)
      for (a, b, c) in lh.bins:
        if (a,b) != lh.binAB((a+b)/2) or a >= b:
          echo "a: ",a," b: ",b," c: ",c," ab(mid(a,b)): ",lh.binAB((a+b)/2)
      if lh.tot > 0: (for q in qs: echo "q",q,": ",lh.quantile(q))
    import cligen; dispatch lghist
  else:
    import std/[random, times, strformat]
    when defined danger: randomize()
    const N = 750_000
    var data = newSeq[F4](N)
    const Q = [0.001,0.01,0.05,0.1587,0.25,0.50,0.75,0.8413,0.95,0.99,0.999]
    var res = newSeq[F4](Q.len)
    for i in 0..<N: data[i] = gauss().F4 # rand(0.0 .. 1.0)
    var s = initHis(b=10, n=128)
    let t0 = epochTime()
    for x in data: s.add x
    when mo in [0, 7]: discard s.cdf(0.0) # Done for the s.up refresh effect
    let t1 = epochTime()
    for j, q in Q: res[j] = s.quantile q
    let t2 = epochTime()
    let dtB = (t1 - t0)*1e9/N.float     # Build time
    let dtQ = (t2 - t1)*1e9/Q.len.float # Query time
    for r in res: echo r
    echo &"ns/add: {dtB:.1f}  ns/q: {dtQ:.1f}  space: {s.space} bytes"
