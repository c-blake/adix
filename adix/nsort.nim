## This numeric sort module encapsulates sorting by native numeric keys embedded
## at some offset inside Nim objects of any size (well, <= 256 bytes for char-
## keyed obs, but you should "tag sort" objects > 64B anyway).  This kind of
## interface allows low overhead generality and enables algorithms specialized
## to number types.  Such algorithms are often many times faster than comparison
## sorts.  The algo is roughly counting sort for 1-Byte keys and for \[248\]Byte
## native type keys an LSD radix sort with optional transforms from signed|float
## domains.  This implementation has several sadly rare optimizations.
##
## FIRST, total order and order by digit0 is checked in the first histogramming
## pass to decide if remaining work can be skipped.  Hence, if run on an already
## sorted array, only one read-only pass over the data is done to confirm order.
## Similarly, items already sorted by digit0 induce no re-ordering write phase.
## Reverse order is also detected.  These non-branching integer comparisons add
## little work and potentially save a lot.
##
## SECOND, only 2 counter buffers are ever needed at once - current pass write
## pointers & next pass read histogram.  Using more wastes limited L1/L2 space
## and/or shrinks max histogram size (increasing number of passes).  The buffers
## for these two "alternate" histograms just toggle across passes.  (A several-
## histogram-at-once counting pass can also achieve no excess re-fetches, but at
## higher cache usage.  Cache use is same for 2B keys, but only high whole byte
## constancy can yield skipped 2nd passes.  The best 2B method depends on keys.)
##
## THIRD, histogram details are optimized.  For moderate `n`, prefix summing
## histograms into cumulative distribution functions (really output bin offsets)
## is a dominant cost.  So, this impl does SIMD parallel prefix sum.  This optim
## is more effective as more counters fit into vector registers.  So, this impl
## uses the smallest \[1248\]Byte counter needed for `n` items & takes care to
## align the 2 power of 2-sized counter buffers to maximize vector use.  Last, a
## time cost estimate formula & Newton's method is used to decide pass sizes.
##
## FOURTH, bits that vary across keys are measured (`mask or=(x[i-1] xor x[i])`)
## in the first read-pass.  Key-wise constant bits are zero in said mask.  Since
## an array MUST be sorted by constant key bits, smart digit plans can skip them
## to maybe shrink pass count.  `pext32/pext64` from Intel's Advanced Bit
## Manipulation subset (Haswell 2014) are a cheap way to use the above mask.
## This optimization does not help bitwise non-constant data (other than ease of
## digit formation afforded by `pext`).  However, this impl is structured so
## there is almost no added cost to access the potentially large savings.  Work
## on digit plans is ongoing.  This optimization is novel to my knowledge.  It
## seems small for an academic paper (maybe optimal digit plans would add enough
## to discuss) but likely to have been mentioned offhand already.  I'd cite a
## reference if I could and please cite me if you don't have one.  How well this
## does depends strongly upon bit-entropy of keys.  Eg., time(2) outputs, over
## allocated 8B ints, 64-bit VM addrs & positive floats spanning 1..2 octaves
## may be constant above 12-24 LSbits.  Some samples may even get sorted in just
## one pass!  I'd bet much real-world f32 data is 2pass w/b0=12.
##
## One undone optimization is multi-threads for counting phases.  This can boost
## L3->L1 read throughput by 2-9x (but since scattered writes are much slower
## than streaming reads, overall speed-up is likely limited).  Another maybe
## useful optimization is saving transformed keys in the tmp arrays (but this
## also needs inverse transforms on the final pass & current xforms are already
## cheap compared to L2 or worse bandwidth costs.  So, 5-20% boost, I'd guess.)

when not declared(stdout): import std/syncio
import adix/[cpuCT, cumsum], std/[bitops, math, algorithm]
when defined(cpuPrefetch): import cligen/prefetch # Keep cligen a soft-dep
const nTinySort{.intdefine.} = 24         # Usually 16-32 is good
const hMaxBits{.intdefine.} = 15          #2*counter-size*2**hMaxBits =~ L2 here
const dGiantSort{.intdefine.} = 15 shl 30 #~50% of uncontended DIMM storage

when defined(amd64) and not defined(noSIMD) and x86bmi2 in x86features:
  proc pext(val,msk:uint32):uint32 {.importc:"_pext_u32", header:"x86intrin.h".}
  proc pext(val,msk:uint64):uint64 {.importc:"_pext_u64", header:"x86intrin.h".}
else:
  {.warning: "nsort compiling without BMI2; Likely slow".}
  template pextPortable(Ts, Tu) =
    proc pext(val, msk: Tu): Tu =
      let val = val.Ts
      var msk = msk.Ts
      var bb = 1'u32
      while msk != 0:
        if (val and msk and -msk) != 0: result = result or bb
        msk = msk and (msk - 1)
        bb += bb
  pextPortable(int32, uint32); pextPortable(int64, uint64)

when defined(windows):                          #import alloca
  proc alloca(n:int): pointer{.importc,header:"<malloc.h>".}
else:
  proc alloca(n:int): pointer{.importc,header:"<alloca.h>",header:"<stddef.h>".}
  {.emit: "void *alloca(size_t);".}

type XForm* = enum xfNone, xfRev, xfSgn, xfSgnRev, xfFlt, xfFltRev ## Transforms
type Ui248 = uint16|uint32|uint64                       #2,4,8Byte unsigned nums
type Ui12  = uint8|uint16                               #1,2Byte unsigned nums
var  verb* = 0                                          ## verbosity level

#Generic params: W: key width type uint(8..64); C: counter type uint(8..64)
#gKey uint(8|16|32|64): get a maybe-transformed key out of an object
proc gKey[W](p: uint, off: W; xf: XForm): W {.inline.} =
  let b  = 8 * W.sizeof - 1
  let hi = 1.W shl b
  let w  = cast[ptr W](p + off.uint)[]
  result = case xf
    of xfNone:   w
    of xfRev:    not w
    of xfSgn:         hi xor w                              #2's complement map
    of xfSgnRev: not (hi xor w)
    of xfFlt:         w xor (hi or (1.W + not (w shr b)))   #IEEE formats
    of xfFltRev: not (w xor (hi or (1.W + not (w shr b))))
#NOTE:                Alternatively ^^ = cast[W](-cast[signed(W)](w shr b))

proc gDig[W: uint32|uint64](p: uint; msk, off: W; xf: XForm): W {.inline.} =
  pext(gKey(p, off, xf), msk)                   #gDig: get a digit out of a key

proc gDig(p: uint; msk, off: uint16; xf: XForm): uint16 {.inline.} =
  pext(gKey(p, off, xf).uint32, msk.uint32).uint16

proc swapMemNative[T](x, y: ptr T) {.inline.} =
  var t: T
  t   = x[]
  x[] = y[]
  y[] = t

proc swapMemBuffer(x, y, t: pointer; n: uint) {.inline.} =
  copyMem(t, x, n)
  copyMem(x, y, n)
  copyMem(y, t, n)

proc swapMem(x, y: pointer; n: uint) {.inline.} =
  var t: array[8, int]
  case n
  of  1: swapMemNative(cast[ptr uint8 ](x), cast[ptr uint8 ](y)); return
  of  2: swapMemNative(cast[ptr uint16](x), cast[ptr uint16](y)); return
  of  4: swapMemNative(cast[ptr uint32](x), cast[ptr uint32](y)); return
  of  8: swapMemNative(cast[ptr uint64](x), cast[ptr uint64](y)); return
  of 12: swapMemBuffer(x, y, t.addr, 12); return
  of 16: swapMemBuffer(x, y, t.addr, 16); return
  of 24: swapMemBuffer(x, y, t.addr, 24); return
  of 32: swapMemBuffer(x, y, t.addr, 32); return
  of 40: swapMemBuffer(x, y, t.addr, 40); return
  of 48: swapMemBuffer(x, y, t.addr, 48); return
  of 56: swapMemBuffer(x, y, t.addr, 56); return
  of 64: swapMemBuffer(x, y, t.addr, 64); return
  else:
    let tz = t.sizeof.uint
    var x = x
    var y = y
    var n = n
    while n > 0:
      if n >= tz:
        swapMemBuffer(x, y, t[0].addr, tz)
        x = cast[pointer](cast[uint](x) + tz)
        y = cast[pointer](cast[uint](x) + tz)
        n -= tz
      else:
        swapMemBuffer(x, y, t[0].addr, n)
        break

proc insSort[W](obs: pointer; n, sz: int, off: W; xf: XForm): pointer =
  let sz = sz.uint                              #Good old insertion sort
  var l = cast[uint](obs)
  let n = l + sz * n.uint
  var m = l + sz
  while m < n:
    l = m
    while l > cast[uint](obs) and gKey(l, off, xf) < gKey(l - sz, off, xf):
      swapMem(cast[pointer](l), cast[pointer](l - sz), sz)
      l.dec sz.int
    m.inc sz.int
  return obs

template allocHisto[C](h: var ptr C, n: int) =
  #Extra space/shifting shenanigans ensure optimal alignment & span for `csum`.
  let x = cast[uint](alloca((n + 1 + 64 div C.sizeof) * C.sizeof))
  h = cast[ptr C](x + ((x + (64 - 2 * C.sizeof)) and 63))

proc `[]`[C, W](h: ptr C, i: W): var C {.inline.} =     #Histo&array accessors
  cast[ptr C](cast[uint](h) + i.uint * C.sizeof.uint)[]
template A(i: untyped): untyped {.dirty.} = cast[uint](obs) + (i * sz).uint
template B(i: untyped): untyped {.dirty.} = cast[uint](tmp) + (i.int * sz).uint

proc cntSort[W: Ui12; C](obs, tmp: pointer; n, sz: int; off: W; xf: XForm;
                         cnt: C): pointer =
  #1-histo+1RW pass counting sort suitable for narrow key types.  For input in
  #strict reverse order, mutates nothing and returns -1. Caller should reverse.
  var needAny = false
  var reverse = true
  var h: ptr C                                  #Buf ptr for histo/output ptrs
  const nH = 1 shl (8 * W.sizeof)
  allocHisto h, nH
  zeroMem h, (nH + 1).int * C.sizeof
  var key0 = gKey(A(0), off, xf)
  h[key0 + 1].inc                               #Count 1st digit for 1st key
  for i in 1 ..< n:                             #**UNCONDITIONAL PASS 0Read**
    let key = gKey(A(i).uint, off, xf)          #**TO COLLECT METADATA(KEYS)**
    needAny = needAny or  (key0 > key)          #Accum tests on whole key order
    reverse = reverse and (key < key0)          #reverse unstable => Strict<
    h[key + 1].inc                              #Count digit
    key0 = key                                  #Save for next loop
  if not needAny: return obs                    #Already sorted
  if reverse: return cast[pointer](-1)          #Tell caller quick in-place rev
  #+ 1 here (w/shift&extra alloc) makes loop wholly SSE/AVX for our pow-of-2 szs
  cumsum h[1].addr, nH + 1                      #Cvt histo->output ptrs
  for i in 0 ..< n:
    let key = gKey(A(i), off, xf)
    let outAddr = B(h[key])
    when defined(cpuPrefetch): prefetch cast[pointer](outAddr+2*sz.uint),pfWrite
    copyMem cast[pointer](outAddr), cast[pointer](A(i)), sz
    h[key].inc                                  #Advance output pointer
  return cast[pointer](B(0))

proc byte0(key: uint16): uint16 {.inline.} = key and 255
proc byte1(key: uint16): uint16 {.inline.} = key shr 8
proc radix2[C](obs, tmp: pointer; n, sz: int; off: uint16; xf: XForm;
               cnt: C): pointer =
  #Only time we know in advance we can do 2pass radix is 2Byte keys,Small-ish n.
  #Even b0=9 `radix` pass does more cumsum work, BUT constant bits higher than
  #2nd byte boundary COULD allow 2nd pass skip. => This is trickily suboptimal.
  var obs = obs; var tmp = tmp
  var needAny, need0, need1: bool               #Default init to false
  var reverse = true
  var h: array[2, ptr C]                        #Buf ptrs for histos/output ptrs
  allocHisto h[0], 256; zeroMem h[0], (256 + 1).int * C.sizeof
  allocHisto h[1], 256; zeroMem h[1], (256 + 1).int * C.sizeof
  var key0  = gKey(A(0), off, xf)               #Get 1st key
  var dig00 = byte0(key0); h[0][dig00 + 1].inc  #Get & count byte0 dig
  var dig01 = byte1(key0); h[1][dig01 + 1].inc  #Get & count byte1 dig
  for i in 1 ..< n:                             #**UNCONDITIONAL PASS 0Read**
    let key  = gKey(A(i).uint, off, xf)         #**TO COLLECT METADATA(KEYS)**
    let dig0 = byte0(key); h[0][dig0 + 1].inc   #Get & count byte0 digit
    let dig1 = byte1(key); h[1][dig1 + 1].inc   #Get & count byte1 digit
    needAny  = needAny or  (key0 > key)         #Accum tests on whole key order
    reverse  = reverse and (key < key0)         #reverse unstable => STRICT<
    need0    = need0   or  (dig00 > dig0)       #Accum tests on byte0 ORDER
    need1    = need1   or  (dig01 != dig1)      #Accum tests on byte1 CONSTANCY
    key0 = key; dig00 = dig0; dig01 = dig1      #Save for next loop
  if not needAny: return obs                    #Already sorted
  if reverse: return cast[pointer](-1)          #Tell caller quick in-place rev
  template pass(p: int, dig: auto) {.dirty.} =
    #256 + 1 here is to make span 256 to get pure vector operations in cumsum.
    cumsum h[p][1].addr, (256 + 1).uint         #Convert counts -> output ptrs
    for i in 0 ..< n:                           #Poke each A-record into its..
      let b = dig(gKey(A(i).uint, off, xf))     #..appropriate spot in B()
      let outAddr = B(h[p][b])
      when defined(cpuPrefetch):prefetch cast[pointer](outAddr+2*sz.uint),pfWrite
      copyMem cast[pointer](outAddr), cast[pointer](A(i)), sz
      h[p][b].inc                               #Advance B/output pointer
  if need0:                                     #Only if necessary..
    pass(0, byte0)                              #..pass0 re-order phase
    if need1:                                   #Only if pass1 will happen:
      swap tmp, obs                             #..swap buf ptrs under A(),B()
  if need1:                                     #byte1 varies across array
    pass(1, byte1)                              #..So do pass1 re-order phase
  return cast[pointer](B(0))                    #Last cpy dst always ret.value

#To decide histogram size we optimize a run-time formula for b-bit histograms.
#T(b) =~ T(csum) + T(sweep&poke&count) = BITS/b*(cB*2^b + 2*dsz/mbw) *IF* we
#neglect the non-differentiable scaling of cntBins tm w/histogram size (ie. cap
#at ~L1).  Ie., T=(x+ye^b)/b where x=BITS*(2*dsz/mbw), y=BITS*cB*ln2.  dT/db = 0
#= -x/b^2 + y(e^b/b-e^b/b^2) => (b-1)e^b = x/y = 2*dsz/mbw/cB/ln2. b>>1 => b =~
#LambertW(x/y); Newton's Method b -= f(b)/f'(b) gives f'(b) = (b-1)e^b + e^b =
#be^b.  So, f(b)/f'(b) = ((b-1)e^b - x/y)/(be^b); x/y = 2*dsz/mbw/cB/ln2
proc optimalB(dsz: float64): float64 =        #dsz in bytes; membw in bytes/ns
  proc membw(dsz: float32): float32 =         #This is the average of streaming
    if dsz < (1 shl 15).float32: return 90.0  #..read + random writes which I
    if dsz < (1 shl 17).float32: return 45.0  #..take to be 1/2 streaming read.
    if dsz < (1 shl 23).float32: return 30.0  #..It would be best, though costly
    return 15.0                               #..to calibrate @run-time.
  result = 6.0                  #For cB, 256 elts in 16 loops of ~6 cyc/loop =
  var db = 1.0                  #.. 256/96 = 2.5 elt/cyc =~ 0.1 elt/ns @4GHz.
  let xOvrY = 2 * dsz / membw(dsz) / 0.1 / 0.693147
  while db.abs > 1e-4:
    db = (1.0 - 1.0 / result) - xOvrY * exp(-result) / result
    result -= db
  result = min(result, hMaxBits)    #L2 cap could be 13..16 given CPU/sys.load

proc nDigits[W](digit, nH: var array[11, W]; mask, nLeft, nD: W): W {.inline.} =
  let b = nLeft div nD
  let m = nLeft mod nD
  for j in 1 .. nD.uint:                        #Set ALL nD histo sizes
    nH[j] = b + (if j > m: 0 else: 1)           #Non-0 mod --> earlier passes
  var k = 0.W
  digit[nD] = mask
  for j in 1 ..< nD.uint:                       #Allocate bits to passes [<nD]
    digit[j] = 0
    var used = 0.W
    while used < nH[j] and k < 8 * W.sizeof:
      let bit = 1.W shl k
      if (mask and bit) != 0:
        digit[j] = digit[j] or bit
        used.inc
      k.inc
    digit[nD] = digit[nD] xor digit[j]          #Accum [nD] mask as we go
  return nD + 1

proc digitPlan[W: Ui248](digit, nH: var array[11, W]; mask: W; b,n,sz,cz: int):
       W {.inline.} =
  let L1    = 32768
  let alt   = nH[0].int * cz
  let dsz2  = 2 * n * sz
  let mask  = mask and not (nH[0] - 1)          #Mask out already counted bits
  let nLeft = popcount(mask).W                  #Distrib rest,balanced pass&hist
  if nLeft == 0:
    return 1
  if nLeft <= b.W or ((1 shl nLeft.int) * cz.int + alt < L1 and dsz2 > L1):
    digit[1] = mask                             #Can fit h[], but not data in L1
    nH[1] = nLeft               #Initially nH is the number of bits NOT entries.
    return 2
  if verb > 1: stdout.write " nLeft: ",nLeft," b: ",b," 2go: ",max(2.W, W(nLeft.float/b.float))
  return nDigits(digit, nH, mask, nLeft, max(2.W, W(nLeft.float / b.float)))

proc radix[W: Ui248; C](obs, tmp: pointer; n, sz: int; off: W; xf: XForm;
                        cnt: C, b=0, b0=0): pointer =
  #This is a multi-pass radix sort that adapts to bitwise constancy.
  var obs = obs; var tmp = tmp
  var digit, nH: array[11, W]
  var mask: W
  var needAny, need0wr: bool                    #Default init to false
  var reverse = true
  var h: array[11, ptr C]                       #Buf ptrs for histos/output ptrs
  nH[0] = (1 shl (if b0 > 0: b0 else: b)).W
  allocHisto h[0], (1 shl hMaxBits)       #Histo toggle=>Alloc MAX-MAX(digPlan)
  zeroMem h[0], (nH[0] + 1).int * C.sizeof
  var key0 = gKey(A(0), off, xf)
  var dig0 = key0 and (nH[0] - 1)
  h[0][dig0 + 1].inc                            #Count 1st digit for 1st key
  for i in 1 ..< n:                             #**UNCONDITIONAL PASS 0Read**
    let key = gKey(A(i).uint, off, xf)          #**TO COLLECT METADATA(KEYS)**
    let dig = key and (nH[0] - 1)               #Extract least signfct digit
    mask    = mask or (key0 xor key)            #Accum varying bits
    needAny = needAny or  (key0 > key)          #Accum tests on whole key order
    need0wr = need0wr or  (dig0 > dig)          #Accum tests on digit order
    reverse = reverse and (key < key0)          #reverse unstable => Strict<
    h[0][dig + 1].inc                           #Count digit
    key0 = key; dig0 = dig                      #Save for next loop
  if not needAny: return obs                    #Already sorted
  if reverse: return cast[pointer](-1)          #Tell caller quick in-place rev
  digit[0] = nH[0] - 1.W
  let nDigit = digitPlan(digit, nH, mask, b, n, sz, C.sizeof).uint #nH[j]<=nH[0]
  var nHmx = 0.W                                #Needed size for alt histo
  for j in 1 ..< nDigit:
    nH[j] = 1.W shl nH[j]                       #Convert from bits to No.Entries
    nHmx = max(nHmx, nH[j])                     #Track max alt. histo size
  if verb > 0: stdout.write " nH: ", nH
  if nHmx > 0: allocHisto h[1], nHmx.int        #Maybe allocate it
  for j in 0 ..< nDigit:                        #Do passes 1 .. nDigit-1
    if j + 1 < nDigit: zeroMem h[j + 1], (nH[j + 1] + 1).int * C.sizeof
    if j > 0 or need0wr:                        #cumsum only if necessary
      cumsum h[j][1].addr, (nH[j] + 1).uint     #Counts -> outPtrs
    for i in 0 ..< n:
      let dig = gDig(A(i), digit[j], off, xf)       #dig for THIS pass
      if j + 1 < nDigit:
        let dNx = gDig(A(i), digit[j + 1], off, xf) #dig for NEXT pass
        h[j + 1][dNx + 1].inc                       #Count digit
      if j > 0 or need0wr:                      #Poke ob into correct out slot
        let outAddr = B(h[j][dig])
        when defined(cpuPrefetch): prefetch(cast[pointer](outAddr + (2 * sz).uint), pfWrite)
        copyMem cast[pointer](outAddr), cast[pointer](A(i)), sz
        h[j][dig].inc                           #Advance output pointer
    if j > 0 or need0wr: swap tmp, obs          #Swap [AB] buffer pointers
    if j + 2 < nDigit: h[j + 2] = h[j]          #Toggle histogram bufs
  return cast[pointer](A(0))                    #Next cpy src always ret.value

proc nsort*(obs, tmp: pointer; n, sz: int; off: uint8; xf: XForm;
            b0=0): pointer =
  ## Specialization of power-user interface for uint8-width keys.
  if n < 1 shl  8: return cntSort(obs, tmp, n, sz, off.uint8, xf, 1'u8 )
  if n < 1 shl 16: return cntSort(obs, tmp, n, sz, off.uint8, xf, 1'u16)
  if n < 1 shl 32: return cntSort(obs, tmp, n, sz, off.uint8, xf, 1'u32)
  return cntSort(obs, tmp, n, sz, off.uint8, xf, 1'u64)

proc nsort*[W: Ui248](obs, tmp: pointer; n, sz: int; off: W; xf: XForm;
                      b0=0): pointer =
  ## A power-user interface that allows skipping a final copy back to `obs`.
  ## It uses `n` to decide what sort algo to use.  Returns -1 if reversed.
  let b = max(6, (optimalB((n * sz).float64) + 0.5).int)    #6=>~1 SSE csum loop
  if n <= 1: return obs                                              #In-order!
  if n <= nTinySort: return insSort(obs, n, sz, off, xf)             #Tiny n
  when W is uint16:
    if b0 > 0 or b > 8: #Use adaptive radix when caller tells us b0 or b big.
      if n < 1 shl  8: return radix(obs, tmp, n, sz, off, xf, 1'u8 , b, b0)
      if n < 1 shl 16: return radix(obs, tmp, n, sz, off, xf, 1'u16, b, b0)
    else:               #..otherwise fixed 2-pass radix.
      if n < 1 shl  8: return radix2(obs, tmp, n, sz, off, xf, 1'u8 )
      if n < 1 shl 16: return radix2(obs, tmp, n, sz, off, xf, 1'u16)
    if n < 1 shl 32:    #XXX This threshold needs tuning love.
      return cntSort(obs, tmp, n, sz, off.uint16, xf, 1'u32)
    else:               #One pass surely pays off w/>= 4 GiObs even w/512kB hist
      return cntSort(obs, tmp, n, sz, off.uint16, xf, 1'u64)
  if n < 1 shl  8: return radix(obs, tmp, n, sz, off, xf, 1'u8 , b, b0)
  if n < 1 shl 16: return radix(obs, tmp, n, sz, off, xf, 1'u16, b, b0)
  if n < 1 shl 32: return radix(obs, tmp, n, sz, off, xf, 1'u32, b, b0)
  return radix(obs, tmp, n, sz, off, xf, 1'u64, b, b0)

proc merge[O,W](obs,tmp: var openArray[O]; iL0,mid,iR0: int; off:W; xf=xfNone)=
  let n  = iR0 - iL0
  var iL = iL0                          #indexes(iL0, iR0)
  var iR = mid
  for i in 0 ..< n:                     #Iterate over length of merge
    if iL < mid and iR == iR0:          #iR @iR0 end=>cpy remaining LHS elts
      copyMem tmp[i].addr, obs[iL].addr, (n - i)*O.sizeof
      break
    elif iR < iR0 and iL == mid:        #iL @iL0 end=>cpy remaining RHS elts
      copyMem tmp[i].addr, obs[iR].addr, (n - i)*O.sizeof
      break
    elif gKey(cast[uint](obs[iL].addr), off, xf) <
         gKey(cast[uint](obs[iR].addr), off, xf):
      tmp[i] = obs[iL]                  #Otherwise add from iL0|iR0 by cmp
      iL.inc
    else:
      tmp[i] = obs[iR]
      iR.inc
  copyMem obs[iL0].addr, tmp[0].addr, n*O.sizeof  #tmp obs -> target

proc msortRec[O, W](obs, tmp: var openArray[O]; left, right: int; off: W;
                    xf=xfNone, b0=0) =
  let n   = right - left
  let mid = left + n div 2
  if obs[0].sizeof * n < dGiantSort:    #BaseCase: radix sort
    let r = nsort(obs[left].addr, tmp[left].addr, n, O.sizeof, off.uint8, xf, b0)
    if r == cast[pointer](-1):
      obs.reverse left, right - 1
    elif r != obs[left].addr:
      copyMem obs[left].addr, tmp[left].addr, obs[0].sizeof * n
  else:                                     #Split in two & sort recursively
    msortRec(obs, tmp, left, mid, off, xf, b0)
    msortRec(obs, tmp, mid, right, off, xf, b0)
    merge(obs, tmp, left, mid, right, off, xf)

proc nsort*[O, W](obs: var openArray[O]; off: W; xf=xfNone, b0=0) =
  ## Radix sort `obs` by `W`-width key at in-object offset `off`,
  ## transforming keys according to `xf`.  `b0` is the number of bits for
  ## the first pass histogram.  0 means use a good default.  Uses temporary
  ## space equal to the size of `obs`.  Does tiled merge sort for big data.
  var tmp = newSeq[O](obs.len)
  msortRec(obs, tmp, 0, obs.len, off, xf, b0)

template nsortByRaw*(x, field: untyped, b0=0): untyped =
  ## Used by `nsortBy` for small objects (or directly by
  ## users who know what they are doing).
  let xfm = if   x[0].field is SomeSignedInt: xfSgn
            elif x[0].field is SomeFloat: xfFlt
            else: xfNone
  case x[0].field.sizeof
  of 1: nsort(x, offsetof(x[0].typeof, field).uint8 , xfm, b0)
  of 2: nsort(x, offsetof(x[0].typeof, field).uint16, xfm, b0)
  of 4: nsort(x, offsetof(x[0].typeof, field).uint32, xfm, b0)
  of 8: nsort(x, offsetof(x[0].typeof, field).uint64, xfm, b0)
  else: raise newException(ValueError,
                           " unsupported field size \"" & $x[0].field.sizeof)

template nsortByTag*(x, field: untyped, b0=0): untyped =
  ## So-called tag sort used by `nsortBy` for large objects (or
  ## directly by users who know what they are doing).
  var tagged = newSeq[tuple[key: typeof(x[0].field), ix: int]](x.len)
  for i in 0 ..< x.len: tagged[i] = (x[i].field, i)
  tagged.nsortByRaw key, b0
  var output = newSeq[typeof(x[0])](x.len)
  for i in 0 ..< x.len: output[i] = x[tagged[i].ix]
  x = output

template nsortBy*(x, field: untyped, b0=0): untyped =
  ## Convenience template around `nsort` proc to reduce typing.  `b0` is the
  ## number of bits for the first pass histogram.  0 means use a good default.
  ##
  ## You can only nsort by one numeric field at a time, but sorts are stable.
  ## Do `x.nsortBy foo; x.nsortBy bar` to do a multi-level sort.
  ##
  ## .. code-block:: nim
  ##   import nsort
  ##   var recs = @[(f: 3.0, age: 50), (f: 6.0, age: 30), (f: 5.0, age: 30)]
  ##   recs.nsortBy f         # Multi-level sort by `age` then `f`.  Invoke
  ##   recs.nsortBy age       # ..in REVERSE order of usual comparison-order.
  ##   for r in recs: echo r  # Right output: @[(5.0,30), (6.0,30), (3.0,50)]
  if x[0].sizeof < 32:
    nsortByRaw(x, field, b0)
  else:
    nsortByTag(x, field, b0)

when isMainModule:
  import std/[random, times, stats, strutils], cligen

  template timeIt(dtMin, norm: float; body: untyped) =
    let t0 = epochTime()
    body
    dtMin = min(dtMin, (epochTime() - t0) * norm)
  proc `$`(rs: RunningStat): string {.inline.} =
    formatFloat(rs.min, ffDecimal, 1) & " +- " &
      formatFloat(rs.standardDeviationS, ffDecimal, 1)

  proc gen[K](x: var openArray[K]; low, range: int) =
    for i in 0 ..< x.len: x[i] = low.K + rand(range.float).K

  type Kind = enum kU1, kS1, kU2, kS2, kU4, kS4, kF4, kU8, kS8, kF8,
                   rU1, rS1, rU2, rS2, rU4, rS4, rF4, rU8, rS8, rF8

  proc doIt[K, W](k:K, w:W, xf: XForm, low=0, range=128, check=false, minN=3,
                  avgN=3, std=false, b0=0, data: seq[uint64], ns: seq[int]) =
    if ns.len < 1: stderr.write "Too few ns\n"; quit(1)
    var o: pointer
    randomize()
    for n in ns:
      let norm = 1e9 / n.float
      let giant = K.sizeof * n >= dGiantSort
      var x = newSeq[K](n)
      var t, y: seq[K]
      if not giant: t = x
      else: echo "giant sort mode"
      if std: y = x
      var dtsMinStd, dtsMinNs: RunningStat
      for avgIt in 1..avgN:
        var dtMinStd = float.high
        var dtMinNs  = float.high
        for minIt in 1..minN:
          if data.len > 0:  #Allow specific data set pass for reproducible debug
            for i in 0 ..< x.len: x[i] = data[i].K
          else:
            x.gen low, range
          if giant: echo "generated ",avgIt," ",minIt
          if verb > 2: stdout.write " x: ", x
          if std:           #stdlib merge sort is many times slower for large N
            y = x
            timeIt(dtMinStd, norm): y.sort
            if giant: echo "stdsort done ",avgIt," ",minIt
          timeIt(dtMinNs, norm):
            if giant: nsort(x, 0.W, xf, b0)
            else:
              o = nsort(x[0].addr, t[0].addr, x.len, x[0].sizeof, 0.W, xf, b0)
              if o == cast[pointer](-1): x.reverse
            if giant: echo "nsort done ",avgIt," ",minIt
          if verb > 2: stdout.write " s: ", (if o == t[0].addr: t else: x)
          if check:
            for i in 1 ..< x.len:
              let a = if o == t[0].addr: t[i-1] else: x[i-1]
              let b = if o == t[0].addr: t[ i ] else: x[ i ]
              if a > b:
                stderr.write "not sorted [", i-1, "] > [", i, "]", "\n"
                break
        dtsMinNs.push dtMinNs
        if std: dtsMinStd.push dtMinStd
      stdout.write "n: ", n, " key: ", $K, " nsort_ns/elt: ", dtsMinNs
      if std: stdout.write " std_ns/elt: ", dtsMinStd
      stdout.write '\n'

  proc tstTm(kind=kF4, vrb=0, low=0, range=128, check=false, minN=3, avgN=3,
             std=false, b0=0, data: seq[uint64], ns: seq[int]) =
    ## This is a test-timing program for the nsort numeric sort module.
    verb = vrb
    case kind
    of kU1: doIt(0'u8 ,0'u8 , xfNone  ,low,range,check,minN,avgN,std,b0,data,ns)
    of kS1: doIt(0'i8 ,0'u8 , xfSgn   ,low,range,check,minN,avgN,std,b0,data,ns)
    of kU2: doIt(0'u16,0'u16, xfNone  ,low,range,check,minN,avgN,std,b0,data,ns)
    of kS2: doIt(0'i16,0'u16, xfSgn   ,low,range,check,minN,avgN,std,b0,data,ns)
    of kU4: doIt(0'u32,0'u32, xfNone  ,low,range,check,minN,avgN,std,b0,data,ns)
    of kS4: doIt(0'i32,0'u32, xfSgn   ,low,range,check,minN,avgN,std,b0,data,ns)
    of kF4: doIt(0'f32,0'u32, xfFlt   ,low,range,check,minN,avgN,std,b0,data,ns)
    of kU8: doIt(0'u64,0'u64, xfNone  ,low,range,check,minN,avgN,std,b0,data,ns)
    of kS8: doIt(0'i64,0'u64, xfSgn   ,low,range,check,minN,avgN,std,b0,data,ns)
    of kF8: doIt(0'f64,0'u64, xfFlt   ,low,range,check,minN,avgN,std,b0,data,ns)
    of rU1: doIt(0'u8 ,0'u8 , xfRev   ,low,range,check,minN,avgN,std,b0,data,ns)
    of rS1: doIt(0'i8 ,0'u8 , xfSgnRev,low,range,check,minN,avgN,std,b0,data,ns)
    of rU2: doIt(0'u16,0'u16, xfRev   ,low,range,check,minN,avgN,std,b0,data,ns)
    of rS2: doIt(0'i16,0'u16, xfSgnRev,low,range,check,minN,avgN,std,b0,data,ns)
    of rU4: doIt(0'u32,0'u32, xfRev   ,low,range,check,minN,avgN,std,b0,data,ns)
    of rS4: doIt(0'i32,0'u32, xfSgnRev,low,range,check,minN,avgN,std,b0,data,ns)
    of rF4: doIt(0'f32,0'u32, xfFltRev,low,range,check,minN,avgN,std,b0,data,ns)
    of rU8: doIt(0'u64,0'u64, xfRev   ,low,range,check,minN,avgN,std,b0,data,ns)
    of rS8: doIt(0'i64,0'u64, xfSgnRev,low,range,check,minN,avgN,std,b0,data,ns)
    of rF8: doIt(0'f64,0'u64, xfFltRev,low,range,check,minN,avgN,std,b0,data,ns)
  dispatch tstTm, positional="ns", implicitDefault = @["data"]
