## This is a "tail digest" {github/tdunning/t-digest|arxiv.org/abs/1902.04023}.
## Counter decs are not possible.  (So, moving quantiles are unsupported.)  In
## my experiments, quantiles are 5X slower than adix/lghisto.  *Tail* quantiles
## do deliver better accuracy for less space (but lghisto already needs little
## local cache space | network BW in *absolute* terms).  There may be a way to
## adapt my B-Tree to speed up the idea. { tDig is also very involved - folks
## just *intuit* histo(ln(x)), but that is a more subjective critique. }

when not declared(addFloat): import std/formatfloat
import std/[math, algorithm]
type
  Scale* = enum scLog           ## Other scales possible, but unimpl here

  Group* = object               ## Group/Cluster/Centroid
    m*: float                   ## mean
    w*: int                     ## weight

  DigesT* = object
    scale*: Scale               ## What scale function to use
    nMerges: int                # counter of merge() to study perf
    min , max: float            # min & max over all sampled
    cpr , pubCpr: float         # Compression factor. Same as paper 1/\delta
    nM  , nT  : int             # next used & next buf group indices
    wTot, wBuf: float           # sum_i mrg[i].w; sum_i buf[i].w
    mrg , buf : seq[Group]      # merged & unmerged groups

func W(c: Group): float = c.w.float

func init*(s: var DigesT, cpr=100.0, scale=scLog, mainLen=0, nBuf=0) =
  s.scale = scale
  s.min   = float.high          #NOTE: nM=nT=wTot=wBuf auto-zeroed by Nim
  s.max   = float.low
  let cpr = max(cpr, 10.0)      # force reasonable value.
  let fudge = if cpr < 30.0: 30.0 else: 10.0
  var size = max(2.0*cpr + fudge, mainLen.float).int # default size
  var nBuf = nBuf
  if nBuf == 0:                 # big buffers good for speed; Returns diminish
    nBuf = 5*size
  nBuf = max(nBuf, 2*size)      # ensure enough space in buffer
  let scl = max(1.0, nBuf.float/size.float - 1.0)
  s.pubCpr = cpr
  s.cpr = sqrt(scl)*cpr         # cpr: max retained groups
  if size.float < s.cpr + fudge: # Adjust if changing cpr may make buf too small
    size = ceil(s.cpr + fudge).int
  nBuf = max(nBuf, 2*size)      # ensure enough space in buffer (maybe again)
  s.mrg.setLen size             # should be one-time space allocations
  s.buf.setLen nBuf

func initDigesT*(cpr=100.0, scale=scLog, mainLen=0, nBuf=0): DigesT =
  result.init cpr, scale, mainLen, nBuf

func space*(s: DigesT): int = (s.nM + s.nT) * Group.sizeof + DigesT.sizeof

func weight*(s: DigesT): int = int(s.wTot + s.wBuf) ## total count represented

func Z(compr, n: float): float = 4.0*ln(n/compr) + 24.0
func k2_norm(compr, n: float): float = compr/Z(compr, n)
func k2_k(q, norm: float): float =
  let q = max(1e-15, min(q, 1 - 1e-15))
  ln(q/(1 - q))*norm
func k2_q(k, norm: float): float = (let w = exp(k/norm); w/(1 + w))

const norms: array[Scale, auto] = [k2_norm]
const qs   : array[Scale, auto] = [k2_q   ]
const ks   : array[Scale, auto] = [k2_k   ]

# Combine existing groups with incoming data & reduce groups by merging.
func merge(s: var DigesT, cpr: float) =
  s.buf[s.nT..<s.nT+s.nM] = s.mrg[0..<s.nM] # arraycopy(mrg,0,s.buf,s.nT,s.nM)
  s.nT   += s.nM
  s.wTot += s.wBuf
  let t = s.buf.len
  s.buf.setLen s.nT
  s.buf.sort proc(a, b: Group): int = system.cmp a.m, b.m
  s.buf.setLen t
  s.nM = 0                      # start by cp least inc value to normal buffer
  s.mrg[s.nM].m = s.buf[0].m
  s.mrg[s.nM].w = s.buf[0].w
  var wSoFar = 0.0
  let norm = norms[s.scale](if cpr < 0: s.cpr else: cpr, s.wTot)
  var k    = ks[s.scale](0, norm)
  var wLim = s.wTot * qs[s.scale](k + 1, norm)
  for i in 1 ..< s.nT:          # weight will contain all zeros after this loop
    let wProp = s.mrg[s.nM].W + s.buf[i].W
    let wProj = wSoFar + wProp
    if wProj <= wLim and not (i == 1 or i == s.nT - 1): # block last group merge
      s.mrg[s.nM].w += s.buf[i].w # next point fits => merge into existing group
      s.mrg[s.nM].m = s.mrg[s.nM].m + (s.buf[i].m -
                                       s.mrg[s.nM].m)*s.buf[i].W/s.mrg[s.nM].W
      s.buf[i].w = 0
    else:                       # didn't fit => mv2next output; Cp 1st group
      wSoFar += s.mrg[s.nM].W
      k    = ks[s.scale](wSoFar/s.wTot, norm)
      wLim = s.wTot * qs[s.scale](k + 1, norm)
      s.nM.inc
      if s.nM >= s.mrg.len:
        s.mrg.setLen s.nM + 1; {.cast(noSideEffect).}: echo "AUTO-EXPAND"
        s.buf.setLen s.buf.len + 1
      s.mrg[s.nM].m = s.buf[i].m
      s.mrg[s.nM].w = s.buf[i].w
      s.buf[i].w = 0
  inc s.nM                      # points to next empty cell
  if s.wTot > 0:                # update extreme values
    s.min = min(s.min, s.mrg[0].m)
    s.max = max(s.max, s.mrg[s.nM - 1].m)

func mergeNew*(s: var DigesT, force=false, cpr = -1.0) =
  if s.wTot == 0 and s.wBuf == 0: return
  if force or s.wBuf > 0: # Do merge in reverse @odd times to avoid lo2hi bias.
    s.merge cpr
    s.nMerges.inc
    s.nT = 0
    s.wBuf = 0

func add*(s: var DigesT, x: float, w=1) = ## Main update API
  if isNaN(x): raise newException(ValueError, "cannot add NaN")
  if s.nT >= s.buf.len - s.nM - 1:
    s.mergeNew
  let i = s.nT; inc s.nT
  s.min = min(s.min, x)
  s.max = max(s.max, x)
  s.buf[i].w = w
  s.buf[i].m = x
  s.wBuf += w.float

func compress*(s: var DigesT) = s.mergeNew(true, s.cpr)
  ## best done only when we want to show results to the outside world.

iterator groups*(s: DigesT): Group =
  for i in 0 ..< s.nM: yield Group(m: s.mrg[i].m, w: s.mrg[i].w)

func add*(s: var DigesT, others: var openArray[DigesT]) =
  for other in mitems(others):
    other.compress
    for c in other.groups: s.add(c.m, c.w)

func weightAvgOrd(x1, w1, x2, w2: float): float {.inline.} =
  let x = (x1*w1 + x2*w2)/(w1 + w2)
  return max(x1, min(x, x2))

func weightedAverage(x1, w1, x2, w2: float): float {.inline.} =
  if x1 <= x2: weightAvgOrd(x1, w1, x2, w2) # WeightedAvg of `x1, s1` & `x2, w2`
  else       : weightAvgOrd(x2, w2, x1, w1) # Guaranteed on `[x1, x2]`

func quantile*(s: var DigesT, q: float): float =
  if q < 0.0 or q > 1.0:
    raise newException(ValueError, "q must be on [0,1], not " & $q)
  s.mergeNew
  if s.nM == 0: return NaN
  if s.nM == 1: return s.mrg[0].m
  var n = s.nM                      # At least two groups now
  var ix = q * s.wTot.float         # weight units offset we want
  if ix < 1: return s.min           # boundaries; return min|max; likely moot
  # If lo group has >1 sample, still know 1 sample occurred @min => interpol.
  if s.mrg[0].w > 1 and ix < s.mrg[0].W/2.0: # only 1 sample @min => less weight
    return s.min + (ix - 1)/(s.mrg[0].W/2.0 - 1) * (s.mrg[0].m - s.min)
  if ix > s.wTot - 1: return s.max  # likely moot
  # If hi group has >1 sample, still know 1 sample occurred @max => interpol.
  if s.mrg[n-1].w > 1 and s.wTot - ix <= s.mrg[n-1].W/2.0:
    return s.max - (s.wTot-ix-1)/(s.mrg[n-1].W/2.0 - 1)*(s.max - s.mrg[n-1].m)
  var wSoFar = s.mrg[0].W/2.0       # between exVals, interpol betw groups
  for i in 0 ..< n - 1:
    let dw = float(s.mrg[i].w + s.mrg[i+1].w)/2.0
    if wSoFar + dw > ix:            # groups i, i+1 bracket current point
      var leftUnit = 0.0            # check for unit weight
      if s.mrg[i].w == 1:
        if ix - wSoFar < 0.5:
          return s.mrg[i].m         # within the singleton's sphere
        else:
          leftUnit = 0.5
      var rightUnit = 0.0
      if s.mrg[i+1].w == 1:
          if wSoFar + dw - ix <= 0.5:
            return s.mrg[i+1].m     # no interpolation needed near singleton
          rightUnit = 0.5
      let z1 = ix - wSoFar - leftUnit
      let z2 = wSoFar + dw - ix - rightUnit
      return weightedAverage(s.mrg[i].m, z2, s.mrg[i+1].m, z1)
    wSoFar += dw
  # Handled singleton@end above
  let z1 = ix - s.wTot - s.mrg[n-1].W/2.0 # wSoFar =~ s.wTot - s.mrg[n-1].w/2
  let z2 = s.mrg[n-1].W/2.0 - z1          # =>interp out to max value ever seen
  return weightedAverage(s.mrg[n-1].m, z1, s.max, z2)

func cdf*(s: var DigesT, x: float): float =
  if x.isNaN: return NaN
  s.mergeNew
  if s.nM == 0: return NaN      # no data to examine
  if x < s.min: return 0.0      # -inf works fine
  if x > s.max: return 1.0      # +inf works fine
  if s.nM == 1:                 # exactly one group, should have max==min
    let width = s.max - s.min
    if x - s.min <= width: return 0.5      # min & max too close to interpolate
    else: return (x - s.min)/(s.max-s.min) # interpol if weight>0, max != min
  let n = s.nM
  if x < s.mrg[0].m:            # check for the LO TAIL
    let dx = s.mrg[0].m - s.min
    if dx > 0.0:                # do not divide by zero in interpol
      return if x == s.min: 0.5/s.wTot        # sample exactly @min
             else: (1.0 + (x - s.min)/dx * (s.mrg[0].W/2.0 - 1.0))/s.wTot
    else: return 0.0            # should be redundant with the check x < s.min
  if x > s.mrg[n-1].m:          # and the HI TAIL
    let dx = s.max - s.mrg[n-1].m
    if dx > 0.0:
      return if x == s.max: 1.0 - 0.5/s.wTot  # single sample exactly @max
             else: 1.0 - (1.0 + (s.max-x)/dx*(s.mrg[n-1].W/2.0 - 1.0))/s.wTot
    else: return 0.0            # should be redundant with the check x > s.max
  var wSoFar = 0.0      # Now mrg[0].m<=x<=mrg[n-1].m >= 2 groups; either >=1
  for i in 0 ..< n-1:   # consecutive groups all @exactly x OR c0 < x < c1
    if s.mrg[i].m == x:         # wSoFar does not yet include s.mrg[i].w
      var dw = 0.0              # Have >=1 groups @x
      for j in i ..< n:         # treat as 1, accumulating weight in dw
        dw += s.mrg[i].W
        if s.mrg[i].m != x: break
      return (wSoFar + dw/2.0)/s.wTot
    elif s.mrg[i].m <= x and x < s.mrg[i+1].m: # betw groups
      if s.mrg[i+1].m - s.mrg[i].m > 0.0: # handle FP issues
        var loExclW = 0.0       # Singleton groups have all weight @mean
        var hiExclW = 0.0       # & should not be smoothed/interpolated.
        if s.mrg[i].w == 1:
          if s.mrg[i+1].w == 1: # 2 singletons=>no interpol; lo in, hi out
            return (wSoFar + 1.0)/s.wTot
          else:
            loExclW = 0.5
        elif s.mrg[i+1].w == 1:
          hiExclW = 0.5
        let dw = float(s.mrg[i].w + s.mrg[i+1].w)/2.0
        let lo = s.mrg[i].m     # adjust endpoints for any singleton
        let hi = s.mrg[i+1].m   # adjusts have limited effect on endpoints
        let dwNoSingleton = dw - loExclW - hiExclW
        let base = wSoFar + s.mrg[i].W/2.0 + loExclW
        return (base + dwNoSingleton * (x - lo)/(hi - lo))/s.wTot
      else:                     # distinct but too close for safe interpolation
        return (wSoFar + float(s.mrg[i].w + s.mrg[i+1].w)/2.0)/s.wTot
    else:
      wSoFar += s.mrg[i].W
  1.0 - 0.5/s.wTot

when isMainModule:
  when defined(test):
    import std/[os, strutils] # Helpful to run against: -12 -8 -4 -1 0 1 4 8 12
    var s = initDigesT(a=0.125, b=10.0, n=8)
    for i in 1 .. paramCount(): s.add parseFloat(paramStr(i))
    for q in [0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99]: echo s.quantile(q)
    echo "s: ", s
  else:
    import std/[random, times, strformat]
    when defined danger: randomize()
    const N = 750_000
    var data = newSeq[float](N)
    const Q = [0.001,0.01,0.05,0.1587,0.25,0.50,0.75,0.8413,0.95,0.99,0.999]
    var res = newSeq[float](Q.len)
    for i in 0..<N: data[i] = gauss() # rand(0.0 .. 1.0)
    var s = initDigesT()
    let t0 = epochTime()
    for x in data: s.add x
    let t1 = epochTime()
    for j, q in Q: res[j] = s.quantile(q)
    let t2 = epochTime()
    let dtB = (t1 - t0)*1e9/N.float     # Build time
    let dtQ = (t2 - t1)*1e9/Q.len.float # Query time
    for r in res: echo r
    echo &"ns/add: {dtB:.1f}  ns/q: {dtQ:.1f}  space: {s.space} bytes"
