## FastIEEESinglePrecNaturalLogAbs; Just arctanh Taylor@1. Was 5X fastr'n mid00s
## x87. On SkyLake/glibc2.40/gcc14 ~1.1-1.4X faster; ARM64glibc somehow(fastHW?)
## ~4X faster.  Unsure about Win/OSX. See news.ycombinator.com/item?id=40758562
type f4s {.packed.} = object            # De-structuring object for IEEE-single
  frac1 {.bitsize: 16}: cuint           # Little-Endian format only right now
  frac0 {.bitsize:  7}: cuint           # `cuint` should make Nim use `unsigned`
  expo  {.bitsize:  8}: cuint           #..for expressions like `expo-126`. Not
  sign  {.bitsize:  1}: cuint           #..sure where this is documented.

const r1_2 = 0.70710678118654752440f64
const LN2  = 0.69314718055994530942f64
const LNr2 = 0.34657359027997265471f64

func lnaSeries(s: float): float {.inline.} =          # Worst case accuracies:
  when defined o3: 2.0/3.0*s + 2.0                            # 11.22 rel bits
  elif defined o5: (0.4*s + 2.0/3.0)*s + 2.0                  # 17.305 rel bits
  elif defined o7: ((2.0/7.0*s + 0.4)*s + 2.0/3.0)*s + 2.0    # 22.774 rel bits
  elif defined o11: # 24.0000002 rel bits
    ((((2.0/11.0*s + 2.0/9.0)*s + 2.0/7.0)*s + 0.4)*s + 2.0/3.0)*s + 2.0
  else: (((2.0/9.0*s + 2.0/7.0)*s + 0.4)*s + 2.0/3.0)*s + 2.0 # 23.97 rel bits

func lna*(x: float32): float32 {.inline.} =
  ## Return fast,approx Natural Log(Abs(x)) { 'a' for a)bs | a)pprox } by ATanH
  var x = x
  let p = cast[ptr f4s](x.addr)
  p.sign = 0                                    # force x to be positive
  let e = (p.expo.cint - 127).float*LN2         # ln(x*2^y) == ln(x) + y*ln2
  p.expo = 127                                  # force x to [1, 2)
  if x > 1.88f32:
    let y = x.float*0.5 - 1.0
    e + LN2 + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
  elif x < 1.06f32:
    let y = x.float - 1.0
    e       + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
  else:
    let d = x.float * r1_2                      # x -> dbl [sqrt(1/2), sqrt2)
    let r = (d.float - 1.0)/(d.float + 1.0)     # r for r)atio of -1/+1
    let s = r*r                                 # s for s)quare
    float32(e + LNr2 + r*s.lnaSeries) # (1.37288 +- 0.00003)X faster on SkyLake

when isMainModule:
  when defined(bench):
    import std/[times, math, formatFloat]
    var sum = 0.0; var n = 0
    let t0 = epochTime()
    for i in 0 .. (1u64 shl 32) - 1:
      var i = uint32(i)
      let x = cast[ptr float32](i.addr)[]
      if x.isNaN: continue
      if x == 0.0f32: continue            # -inf
      when defined(stdlib): (let l = ln(abs(x)))
      else                : (let l = lna(x))
      inc n
      if l.isNaN: continue
      if l == Inf: echo "Inf@x: ", x
      sum += l
    echo "sum: ", sum, " in ", epochTime() - t0, " seconds; n: ", n
  else:
   func lnaT*(x: float32): float32 {.inline.} =
    var x = x
    let p = cast[ptr f4s](x.addr)
    p.sign = 0                                    # force x to be positive
    let e = (p.expo.cint - 127).float*LN2         # ln(x*2^y) == ln(x) + y*ln2
    p.expo = 127                                  # force x to [1, 2)
    if x > 1.88f32:
      let y = x.float*0.5 - 1.0
      {.cast(noSideEffect).}: echo "x: ",x," e: ",e," y: ",y
      e + LN2 + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
    elif x < 1.06f32:
      let y = x.float - 1.0
      {.cast(noSideEffect).}: echo "X: ",x," E: ",e," Y: ",y
      e       + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
    else:
      let d = x.float * r1_2                      # x -> dbl [sqrt(1/2), sqrt2)
      let r = (d.float - 1.0)/(d.float + 1.0)     # r for r)atio of -1/+1
      let s = r*r                                 # s for s)quare
      {.cast(noSideEffect).}: echo "x: ",x," d: ",d," e: ",e," r: ",r," s: ",s,
                                   " iM: ",s.lnaSeries
    float32(e + LNr2 + r*s.lnaSeries) # (1.37288 +- 0.00003)X faster on SkyLake
    when not declared(stdout): import std/[syncio, formatFloat]
    import std/[math, heapqueue]
    const n = 15  # echo top <ThisMany> absolute & relative errors
    var abErr, rlErr: HeapQueue[(float, float32, float32, float32)]
    for i in 0 .. (1u64 shl 32) - 1:
      var i = uint32(i)
      let x = cast[ptr float32](i.addr)[]
      if x.isNaN: continue
      if x < 0: continue
      if x == float32.low: continue
      if x == float32.high: continue
      if x < 1.1754944e-38: continue    # under IEEE single limit
      if x > 1.7014118e38: continue     # above IEEE single limit
      if x < 0.5: continue # accelerators
      if x > 2.0: continue # accelerators
      if x == 0.0f32: continue          # -inf
      if x == 1.0f32: continue          # exactly 0.0
      let accu = ln(abs(x.float))       #     let accu = lnf(x.float)
      let appr = lna(x).float
      let aerr = (abs(appr - accu), x, accu.float32, appr.float32)
      if abErr.len < n          : abErr.push(aerr)
      elif aerr[0] > abErr[0][0]: discard abErr.replace(aerr)
      let rerr = (abs(appr/accu - 1.0), x, accu.float32, appr.float32)
      if rlErr.len < n          : rlErr.push(rerr)
      elif rerr[0] > rlErr[0][0]: discard rlErr.replace(rerr)
      if (i and 0x00FFFFFFu32) == 0: stdout.write "."; stdout.flushFile
    echo "\n"
    echo "abs: ";(while abErr.len>0:(let e=abErr.pop;echo " ",e,lnaT(e[1])))
    echo "rel: ";(while rlErr.len>0:(let e=rlErr.pop;echo " ",e,lnaT(e[1])))
#[ b=(chrt 99 taskset -c 2-3 env -i HOME=/u/cb PATH=/u/cb/bin:/usr/local/bin:/usr/bin)
for p in *Fast *Std *FastFIM *StdFIM *FastFM *StdFM;{ec $p;repeat 2 nor 0 $b ./$p}
AlderLake (i7-1370P @5.2 GHz); frq v
  lnaFast - 1.194x faster
  sum: 1652640836.519009 in 10.62613010406494 seconds; n: 4278190080
  sum: 1652640836.519009 in 10.62076020240784 seconds; n: 4278190080
  lnaStd
  sum: inf in 12.71785449981689 seconds; n: 4278190080
  sum: inf in 12.68324708938599 seconds; n: 4278190080
  lnaFastFIM - 1.174x faster
  sum: 1652640836.519009 in 10.93485021591187 seconds; n: 4278190080
  sum: 1652640836.519009 in 11.22672414779663 seconds; n: 4278190080
  lnaStdFIM
  sum: inf in 12.90901017189026 seconds; n: 4278190080
  sum: inf in 12.83666086196899 seconds; n: 4278190080
  lnaFastFM - 1.104x faster
  sum: 1652640836.519009 in 9.682320833206177 seconds; n: 4278190080
  sum: 1652640836.519009 in 9.732645034790039 seconds; n: 4278190080
  lnaStdFM
  sum: inf in 10.70303058624268 seconds; n: 4278190080
  sum: inf in 10.68658685684204 seconds; n: 4278190080
SkyLake (i7-6700k@4.0GHz): frq f
  lnaFast - 1.373x faster
  sum: 1652640836.519009 in 19.2596070766449 seconds; n: 4278190080
  sum: 1652640836.519009 in 19.25886845588684 seconds; n: 4278190080
  lnaStd
  sum: inf in 26.43976330757141 seconds; n: 4278190080
  sum: inf in 26.44279456138611 seconds; n: 4278190080
  lnaFastFIM - 1.138x faster
  sum: 1652640836.519009 in 19.66579365730286 seconds; n: 4278190080
  sum: 1652640836.519009 in 19.66761422157288 seconds; n: 4278190080
  lnaStdFIM
  sum: inf in 22.48359608650208 seconds; n: 4278190080
  sum: inf in 22.38936853408813 seconds; n: 4278190080
  lnaFastFM - 1.388x faster
  sum: 1652640836.519009 in 15.90760946273804 seconds; n: 4278190080
  sum: 1652640836.519009 in 15.90723752975464 seconds; n: 4278190080
  lnaStdFM
  sum: inf in 22.09396910667419 seconds; n: 4278190080
  sum: inf in 22.07609820365906 seconds; n: 4278190080 ]#
