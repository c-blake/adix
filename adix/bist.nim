##[ Binary Indexed Sum Tree (BIST); Fenwick proposed "BIT" but that A) collides
w/many uses B) takes partial (S)ums as implied, but explicit is better (though
products can work) and C) does not rhyme with "dist" (for distribution - what it
is mostly about).  While Inet has tutorials, to my knowledge no one (yet)
collects all these algos in one place.  Fenwick1994 itself messed up `invCDF`,
correcting w/a tech report a year later.  This code only allocates needed space
& uses 0-based array indexing.  See https://en.wikipedia.org/wiki/Fenwick_tree

The idea of a standard binary heap with `kids(k)@[2k],[2k+1]` for dynamic
distributions goes back to Wong&Easton 1980 (or earlier?).  Fenwick's clever
index encoding/overlaid trees idea allows using 1/4 to 1/2 that space (only max
index+1 array elements vs `2*lgCeil(n)`), a constant factor improvement.  Good
explanations truly need figures, as in the original Fenwick paper | Wikipedia.
  
The `Bist[T]` type in this module is generic over the type of counters used for
partial sums|counts.  For few total items, you can use a `Bist[uint8]` while for
many you want to use `Bist[uint32]`.  This can be space-optimized up to 2X
further with `adix/sequint` specialized to store an array of B-bit counters.
Ranked B-trees are faster for >24..28-bit index spaces as L3 CPU caching fails,
but needing >7..8 decimal dynamic ranges is also rare. ]##
when not declared assert: import std/assertions
import xlang, bitop # cfor, `>>=`, `&=`; `ceilPow2`

type Bist*[T: SomeInteger] = object ## A razor thin wrapper around `seq[T]`
  tot*: int           # total counted population, via history of inc(i, d)
  data*: seq[T]       # The Fenwick array/BIST; Relevant seq ops pass through

proc init*[T](t: var Bist[T], len: int) = t.data.setLen len
proc initBist*[T](len: int): Bist[T] = result.init len
proc len*[T](t: Bist[T]): int = t.data.len
func space*[T](t: Bist[T]): int = t.sizeof + t.data.len*T.sizeof
proc count*[T](t: Bist[T]): int = t.tot
proc `[]`*[T](t: Bist[T], i: int): T = t.data[i]
proc `[]`*[T](t: var Bist[T], i: int): var T = t.data[i]
proc `[]=`*[T](t: var Bist[T], i: int, x: T) = t.data[i] = x
proc clear*[T](t: var Bist[T]) =
  t.tot = 0; zeroMem t.data[0].addr, t.len*T.sizeof

proc inc*[T](t: var Bist[T]; i, d: SomeInteger) =
  ## Adjust for count update; Eg. inc(T,i,-1) decs count@i; Tm ~ 1/2..3/4 lg n
  t.tot += int(d)                                 #Likely T unsigned, d signed
  cfor (var i = i.int), i < t.len, i |= i + 1:    #Go down update tree
    t[i] = T(int(t[i]) + d.int)                   #Likely T unsigned, d signed

proc cdf*[T](t: Bist[T], i: int): T =
  ## INCLUSIVE `sum(pmf[0..i])`, (rank,EDF,prefix sum,scan,..); Tm~1 bits in `i`
  cfor (var i = i + 1), i > 0, i &= i - 1:        #Go up interrogation tree
    result += t[i - 1]

proc pmf*[T](t: Bist[T], i: int): T =
  ## Probability Mass Function @i;  Avg Tm ~ 2 probes; Max Tm ~ lg n
  result = t[i]
  cfor (var mask = 1), (i and mask) == mask, mask <<= 1:
    result -= t[i - mask]           #while LSB==1: subtract & mv up tree

proc fromCnts*[T](t: var Bist[T]) =
  ## In-place bulk convert/reformat `t[]` from counts to BIST; Max time `~1*n`.
  t.tot = 0
  for i in 0 ..< t.len:
    t.tot += int(t[i])
    let j = i or (i + 1)
    if j < t.len:
      t[j] += t[i]

proc toCnts*[T](t: var Bist[T]) =
  ## In-place bulk convert/reformat `t[]` from BIST to counts; Max time ~1*n
  ## *Unlike the others, this routine only works for power of 2-sized arrays*.
  cfor (var i = t.len), i != 0, i >>= 1:      #Long strides give ~n inner loops.
    cfor (var j = 2*i - 1), j < t.len, j += 2*i:  #*Might* be slower than just
      t[j] -= t[j - i]                            #..looping & calling `pmf`.

proc counts*[T](t: Bist[T]): seq[T] = ## Return classic PMF from read-only BIST
  result.setLen t.len; for i in 0 ..< t.len: result[i] = t.pmf(i)

proc cumuls*[T](t: Bist[T]): seq[T] = ## Return classic CDF from read-only BIST
  result = t.counts; for i in 1 ..< t.len: result[i] += result[i - 1] # .cumsum?

proc `$`*[T](t: Bist[T]): string = "tot: " & $t.count & " pmf: " & $t.counts

proc invCDF*[T](t: Bist[T], s: T; s0: var T): int =
  ## For `0 < s <= tot`, bracket ECDF jump `>= s`.  I.e. find `i0, s0` so `s0 =
  ## sum(..< i0) < s yet sum(..i0) >= s` in `lgCeil n` array probes.
  assert 0<s.int and s.int<=t.tot,"invCDF(Bist[T]) called with out of range sum"
  var c = s - 1                         #NOTE: s==0 | s > tot are invalid inputs
  cfor (var half = t.len.ceilPow2 shr 1), half != 0, half >>= 1:
    var mid = result + half - 1
    if mid < t.data.len and t[mid] <= c:
      c -= t[mid]
      result = mid + 1
  s0 = s - c - 1

proc invCDF*[T](t: Bist[T], s: T): (int, T) = result[0] = t.invCDF(s, result[1])
  ## For `0 < s <= tot` return `(i0,s0)` so `sum(..<i0)=s0 < s and sum(..i0)>=s`

proc invCDF*[T](t: Bist[T]; s: T; s0, s1: var T): int =
  ## For `0 < s <= tot`, find `i0,s0,s1` so `s0 < s <= s1` and `s0+pmf(i0)==s1`.
  result = t.invCDF(s, s0)
  s1 = s0 + t.pmf(result)

proc min*[T](t: Bist[T]): int = ## Simple wrapper: invCDF(t, 1)
  var s0: T; t.invCDF(1, s0)

proc max*[T](t: Bist[T]): int = ## Simple wrapper: invCDF(t,t.count).
  var s0: T; t.invCDF(t.tot.T, s0)

proc quantile*[T](t: Bist[T]; q: float; iL,iH: var int): float =
  ## Parzen-interpolated quantile; E.g., q=0.9 => 90th percentile.  ``answer =
  ## result*iL + (1-result)*iH``, but is left to caller to do { in case it is
  ## mapping larger numeric ranges to/from iL,iH }.  Tm ~ ``2*lg(addrSpace)``.
  ## Unlike other (broken!) quantile-interpolation methods, Parzen's connects
  ## midpoints of vertical CDF jumps, not horizontal.  This makes far more sense
  ## with ties, corresponding to age-old "tie mid-ranking" recommendations.
  assert t.tot > 0, "quantile(Bist[T]) requires non-empty bist."
  var sL0, sL1, sH0, sH1: T                 #You probably want to draw a CDF to
  let n  = t.tot.float                      #..fully understand this code.
  let qN = q*n
  if qN <= 0.5    : iL = t.min; iH = 0; return 1 #Early rets for tails; Pure iL
  if qN >= n - 0.5: iL = t.max; iH = 0; return 1 #{Early for body are pure iH.}
  iH = t.invCDF(T(qN + 1.5), sH0, sH1)
  var sMidH = 0.5*float(sH0 + sH1)          #This guess works 90+% of the time..
  if sMidH < qN:                            #..but can fail for large sH1 - sH0.
    if sH1.int < t.tot:                     #When it fails, want next higher bin
      iH    = t.invCDF(sH1 + 1, sH0, sH1)
      sMidH = 0.5*float(sH0 + sH1)
    else: return 0                          #..unless @HIGHEST already=>all iH
  if sH0 == 0: return 0                     #For qN this small, iH = iL = min.
  iL = t.invCDF(sH0, sL0, sL1)              #..Also, cannot call invCDF(0).
  let sMidL = 0.5*float(sL0 + sL1)          #Mid-vertJump(nxtLwrBin) gives line
  (sMidH - qN)/(sMidH - sMidL)

proc quantile*[T](t: Bist[T], q: float): float =
  ## Parzen-interpolated quantile when no caller index mapping is needed
  var iL, iH: int
  let fL = t.quantile(q, iL, iH)
  fL*iL.float + (1 - fL)*iH.float

when isMainModule:
  import cligen, std/strutils
  when not declared(addFloat): import std/formatfloat
  type ct = uint16
  proc tbist(num=9, verb=false, parzen=false, thresh=0.03, args: seq[int]): int=
    ## E.g. `tbist $(echo 0 2 4 4 4 6 6 6 6 8 | tr \  \\n | shuf)`.  Exit status
    ## is bitmask of PMF|CDF|invCDF|Extremes|discontinuousQtls|badFrom|badToCnts.
    result = 0    #Set to non-zero on failure for easy halt of randomized tests.
    var cntR = newSeq[ct](num)                        #Reference count/PMF/histo
    var sumR = newSeq[ct](num)                        #Reference prefix sum/CDF
    var minR = int.high
    var maxR = int.low
    var b    = initBist[ct](num)
    for a in args:                                    #Load up bist & references
      if a < 0   : echo "tbist: ignoring negative ", a     ; continue
      if a >= num: echo "tbist: ignoring out of bounds ", a; continue
      cntR[a].inc                                     #Reference cntR
      minR = min(minR, a)
      maxR = max(maxR, a)
      b.inc(a, +1)
    sumR[0] = cntR[0]                                 #Low-Tech Prefix Sum/CDF
    for i in 1 ..< num:
      sumR[i] = sumR[i-1] + cntR[i]                   #Ref cumulative/pfx sum
    if verb:                                          #Print Table
      echo "i(dec)\ti(bin)\tT\tcount\tcsum"
      for i in 0 ..< num:
        echo "$1\t$2\t$3\t$4\t$5"%[ $i, toBin(i,6), $b[i], $cntR[i], $sumR[i] ]
    for i in 0 ..< b.len:                             #Test pmf()
      if b.pmf(i) != cntR[i]:
        echo "i: ", i, "\tcntR: ", sumR[i], " b.pmf:", b.pmf(i); result |= 1
    for i in 0 ..< b.len:                             #Test cdf()
      if b.cdf(i) != sumR[i]:
        echo "i: ", i, "\tsumR: ", sumR[i], " b.cdf:", b.cdf(i); result |= 2
    for s in 1.ct .. args.len.ct:                     #Test invCDF 4all cumSums
      let (i, s0) = b.invCDF(s)
      let j = i - 1; let s1 = s0 + cntR[i]
      if s1 != sumR[i] or (j >= 0 and s0 != sumR[j]) or not(s0 < s and s <= s1):
        echo "cs: ",s," im1: ",j," s0: ",s0," i: ",i," s1: ",s1; result |= 4
    if b.min != minR: echo "wrong min: ", b.min, " not ", minR; result |= 8
    if b.max != maxR: echo "wrong max: ", b.max, " not ", maxR; result |= 8
    let dq = 1.0/2048.0                               #Test quantile continuity
    var q0 = -1.0; var qP0 = 0.0                      #Take dq as param?
    cfor (var q = 0.0), q <= 1.0, q += dq:
      let qP = b.quantile(q)
      if parzen : echo "P: ", q, " ", qP
      if q0 > -1 and abs(qP - qP0) > thresh:
       result |= 16  #NOTE: Test less objective; Set parzen to assess manually.
       echo "PdisCont: ",q0," -> ",q," ",qP0," -> ",qP," |qP0-qP|: ",abs(qP-qP0)
      q0 = q; qP0 = qP                                #save last loop values
    var t = b; t.data = cntR                          #Bulk Histogram -> BIST
    t.fromCnts
    if b.data != t.data:
      echo "- bad fromCnts chk -"; result |= 32
      for i in 0 ..< b.len: echo "i: ", i, "\tT: ", t[i]
    if num.isPow2:
      b.toCnts                                        #Bulk BIST -> Histogram
      if b.data != cntR:                              #NOTE: `b` is clobbered
        echo "- bad toCnts chk -"; result |= 64
        for i in 0..<b.len: echo "i: ", i, "\tcntR: ", cntR[i], "\tb[i]: ", b[i]
  dispatch tbist, help={"args": "various positive integers",
    "num"   : "allocated space for Fenwick/BIST array",
    "verb"  : "verbosely print the distribution",
    "parzen": "do parzen quantile interpolation",
    "thresh": "Pr diff meaning 'quantile discontinuity'"}
