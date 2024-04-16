## Binary Indexed Sum Tree (BIST); Invented by P.Fenwick in 1994. { Fenwick
## proposed "BIT" but that A) collides with many uses, B) takes partial sums as
## implied, while the trick applies more broadly (e.g.products), and C) does not
## rhyme with "dist" (for distribution, which is what this is mostly about). }
## While the Internet has many tutorials, to my knowledge, no one (yet) collects
## these algorithms all in one place.  Fenwick1994 itself messed up on what we
## here call ``invCDF``, correcting with a tech report a year later.  This
## implementation is also careful to only allocate needed space/handle `[0]`.
##
## The basic idea of a standard binary heap with ``kids(k)@[2k],[2k+1]`` for
## dynamic distributions goes back to Wong&Easton 1980 (or earlier?).  Fenwick's
## clever index encoding/overlaid trees trick allows using 1/4 to 1/2 that space
## (only max index+1 array elements vs ``2*lgCeil(n)``). Meaningful explanations
## really need figures as in the original Fenwick paper or another take over at
##   https://notes.tweakblogs.net/blog/9835/fenwick-trees-demystified.html.
##
## The ``Bist[T]`` type in this module is generic over the type of counters used
## for partial sums|counts.  For few total items, you can use a ``Bist[uint8]``
## while for many you want to use `Bist[uint32]`.  This can be space-optimized
## up to 2X further with a ``sequint`` specialized to store an array of B-bit
## counters.  Also, ranked B-trees start being faster for >28-bit index spaces.
import xlang, bitop # cfor, `>>=`, `&=`
when not declared(assert): import std/assertions

type Bist*[T: SomeInteger] = object ## A razor thin wrapper around `seq[T]`
  data: seq[T]        # The Fenwick array/BIST; Relevant seq ops pass through
  tot: int            # total counted population, via history of inc(i, d)

proc initBist*[T](len: int): Bist[T] {.inline.} = result.data = newSeq[T](len)
proc len*[T](t: Bist[T]): int {.inline.} = t.data.len
func space*[T](t: Bist[T]): int = t.sizeof + t.data.len*T.sizeof
proc count*[T](t: Bist[T]): int {.inline.} = t.tot
proc `[]`*[T](t: Bist[T], i: int): T {.inline.} = t.data[i]
proc `[]`*[T](t: var Bist[T], i: int): var T {.inline.} = t.data[i]
proc `[]=`*[T](t: var Bist[T], i: int, x: T) {.inline.} = t.data[i] = x
proc clear*[T](t: var Bist[T]) {.inline.} =
  t.tot = 0; zeroMem t.data[0].addr, t.len*T.sizeof

proc inc*[T](t: var Bist[T]; i, d: SomeInteger) {.inline.} =
  ## Adjust for count update; Eg. inc(T,i,-1) decs count@i; Tm ~ 1/2..3/4 lg n
  t.tot += int(d)                                 #Likely T unsigned, d signed
  cfor (var i = i.int), i < t.len, i |= i + 1:    #Go down update tree
    t[i] = T(int(t[i]) + d.int)                   #Likely T unsigned, d signed

proc cdf*[T](t: Bist[T], i: int): T {.inline.} =
  ## Inclusive `sum(pmf[0..i])`, (rank,EDF,prefix sum,scan,..); Tm~1 bits in `i`
  cfor (var i = i + 1), i > 0, i &= i - 1:        #Go up interrogation tree
    result += t[i - 1]

proc pmf*[T](t: Bist[T], i: int): T {.inline.} =
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

proc invCDF*[T](t: Bist[T], s: T; s0: var T): int {.inline.} = # ILP?
  ## Find ``i0,s0`` when ``sum(i0..i?)==s1; s0+pmf(i0)==s1`` for ``0<s<=tot``
  ## in ``lgCeil(n)`` array probes.  (sum jumps from ``s0@i0-1`` to ``s1@i0``).
  var c = s - 1                         #NOTE: s==0 is invalid input
  cfor (var half = t.len.ceilPow2 shr 1), half != 0, half >>= 1:
    var mid = result + half - 1
    if mid < t.data.len and t[mid] <= c:
      c -= t[mid]
      result = mid + 1
  s0 = s - c - 1

proc invCDF*[T](t: Bist[T]; s: T; s0, s1: var T): int {.inline.} =
  ## Find ``i0,s0,s1`` in ``sum(i0..i?)==s1; s0+pmf(i0)==s1`` for ``0<s<=tot``.
  result = t.invCDF(s, s0)
  s1 = s0 + t.pmf(result)

proc min*[T](t: Bist[T]): int {.inline.} =
  ## Convenience wrapper returning invCDF(t, 1)
  var s0: T
  result = invCDF(t, 1, s0)

proc max*[T](t: Bist[T]): int {.inline.} =
  ## Convenience wrapper returning invCDF(t, t.count).
  var s0: T
  result = invCDF(t, T(t.tot), s0)

proc quantile*[T](t: Bist[T], q: float; iL, iH: var int): float {.inline.} =
  ## Parzen-interpolated quantile; E.g., q=0.9 => 90th percentile.  ``answer =
  ## result*iL + (1-result)*iH``, but is left to caller to do { in case it is
  ## mapping larger numeric ranges to/from iL,iH }.  Tm ~ ``2*lg(addrSpace)``.
  ## Unlike other (broken!) quantile-interpolation methods, Parzen's connects
  ## midpoints of vertical CDF jumps, not horizontal.  This makes far more sense
  ## with ties, corresponding to age-old "tie mid-ranking" recommendations.
  assert t.tot > 0, "quantile(Bist[T]) requires non-empty bist."
  var sL0, sL1, sH0, sH1: T                 #You probably want to draw a CDF to
  let ni = t.tot                            #..fully understand this code.
  let n  = ni.float
  let qN = q * n
  if qN <= 0.5:                             #Early returns for tails are pure iL
    iL = t.min; iH = 0; return 1.0          #..while early for body are pure iH.
  if qN >= n - 0.5:
    iL = t.max; iH = 0; return 1.0
  iH = invCDF(t, T(qN + 1.5), sH0, sH1)     #This guess works 90+% of the time..
  var sMidH = (sH0 + sH1).float * 0.5
  if sMidH < qN:                            #..but can fail for large sH1 - sH0.
    if int(sH1) < ni:                       #When it fails, want next higher bin
      iH    = invCDF(t, sH1 + 1, sH0, sH1)
      sMidH = (sH0 + sH1).float * 0.5
    else: return 0.0                        #..unless @HIGHEST already=>all iH
  if sH0 == 0: return 0.0                   #For qN this small, iH = iL = min.
  iL = invCDF(t, sH0, sL0, sL1)             #..Also, cannot call invCDF(0).
  let sMidL = (sL0 + sL1).float * 0.5       #Mid-vertJump(nxtLwrBin) gives line
  result = (sMidH - qN) / (sMidH - sMidL)

proc quantile*[T](t: Bist[T], q: float): float {.inline.} =
  ## Parzen-interpolated quantile when no caller index mapping is needed
  var iL, iH: int
  let fL = t.quantile(q, iL, iH)
  fL * iL.float + (1 - fL) * iH.float

when isMainModule:
  import cligen, strutils
  when not declared(addFloat): import std/formatfloat
  type ct = uint16
  proc tbist(verb=false, parzen=false, thresh=0.03, num=16, args: seq[int]):int=
    ## e.g. tbist 0 2 5 5 5 8 8 8 8 11
    result = 0    #Set to non-zero on failure for easy halt of randomized tests.
    var cntR = newSeq[ct](num)
    var sumR = newSeq[ct](num)
    var minR = int.high
    var maxR = int.low
    var b    = initBist[ct](num)
    for a in args:                                    #Load up bist
      if a >= num: echo "tbist: ignoring out of bounds ", a; continue
      cntR[a].inc                                     #Reference cntR
      minR = min(minR, a)
      maxR = max(maxR, a)
      b.inc(a, +1)
    sumR[0] = cntR[0]
    for i in 1 ..< num:
      sumR[i] = sumR[i-1] + cntR[i]                   #Reference cumsum
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
    var s0: ct                                        #Test invCDF()
    for cs in 1.ct .. args.len.ct:
      let i  = b.invCDF(cs, s0)
      let i0 = i - 1; let s1 = s0 + cntR[i]
      if s1 != sumR[i] or (i0 >= 0 and s0 != sumR[i0]):
        echo "cs: ",cs," i0: ",i0," s0: ",s0," i: ",i," s1: ",s1; result |= 4
    if b.min != minR: echo "wrong min: ", b.min, " not ", minR; result |= 8
    if b.max != maxR: echo "wrong max: ", b.max, " not ", maxR; result |= 8
    let dq = 1.0 / 2048.0                             #Test quantile continuity
    var q0 = -1.0; var qP0 = 0.0                      #Take dq as param?
    cfor (var q = 0.0), q <= 1.0, q += dq:
      let qP = b.quantile(q)
      if parzen : echo "P: ", q, " ", qP
      if q0 > -1 and abs(qP - qP0) > thresh:
       result |= 16  #NOTE: Test less objective; Set parzen to assess manually.
       echo "Pdiscont: ",q0," -> ",q," ",qP0," -> ",qP," |qP0-qP|: ",abs(qP-qP0)
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
  dispatch(tbist)
