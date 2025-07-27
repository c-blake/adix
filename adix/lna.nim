## FastIEEESinglePrecNaturalLogAbs; Just arctanh Taylor@1. Was 5X fastr'n mid00s
## x87. On SkyLake/glibc2.40/gcc14 ~1.2-2X faster; ARM64glibc somehow(fastHW?)
## ~4X faster.  Unsure about Win/OSX. See news.ycombinator.com/item?id=40758562
type f4s {.packed.} = object            # De-structuring object for IEEE-single
  frac1 {.bitsize: 16}: cuint           # Little-Endian format only right now
  frac0 {.bitsize:  7}: cuint           # `cuint` should make Nim use `unsigned`
  expo  {.bitsize:  8}: cuint           #..for expressions like `expo-127`. Not
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
  ## 82% of the time & by std ln(1+x) series 12+6=18%. { BUT 0.lna=-88,not -Inf|
  ## +Inf@Inf; Not fixed inline here since outer caller should block|handle 0. }
  var x = x
  let p = cast[ptr f4s](x.addr)
  p.sign = 0                                    # abs(); Force x to be positive
  let e = (p.expo.cint - 127).float*LN2         # ln(x*2^y) == ln(x) + y*ln2
  p.expo = 127                                  # force x to [1, 2)
  if x > 1.88f32:   # Small y in x=2-y: 6 terms of ln(1+y)=Σy^i/i BUT ..
    let y = x.float*0.5 - 1.0                   #..adjusted for NEXT octave.
    e + LN2 + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
  elif x < 1.06f32: # Small y in x=1+y: 6 terms of ln(1+y)=Σy^i/i
    let y = x.float - 1.0
    e       + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
  else:             # 2*atanh(x) = ln((1+x)/(1-x)) = 2*Σx^o/o for odd 'o'
    let d = x.float * r1_2                      # x -> dbl [sqrt(1/2), sqrt2)
    # d=(1+r)/(1-r); (1+r)=(1-r)*d=d-rd; d-rd-r-1; r*(d+1)=d-1; r=(d-1)/(d+1)
    let r = (d.float - 1.0)/(d.float + 1.0)     # r for r)atio of -1/+1
    let s = r*r                                 # s for s)quare
    e + LNr2 + r*s.lnaSeries    # (1.37288 +- 0.00003)X faster on SkyLake

when isMainModule:
  when defined(bench):
    import std/[times, math, strformat]
    var sum0 = 0.0; var sum = 0.0; var n = 0
    let t00 = epochTime()
    for i in 0 .. (1u64 shl 32) - 1:
      var i = uint32(i)
      let x = cast[ptr float32](i.addr)[]
      if x.isNaN: continue
      if x == 0.0f32: continue            # -inf
      inc n
      if not (x.isNaN or 2*x==x): sum0 += x
    let dt0 = epochTime() - t00
    let t0 = epochTime()
    for i in 0 .. (1u64 shl 32) - 1:
      var i = uint32(i)
      let x = cast[ptr float32](i.addr)[]
      if x.isNaN: continue
      if x == 0.0f32: continue            # -inf
      when defined(stdlib): (let l = ln(abs(x)))
      else                : (let l = lna(x))
      inc n
      if not (l.isNaN or 2*x==x): sum += l
    let dt = epochTime() - t0 - dt0
    echo &"sX:{sum0:.2g} sL:{sum:.0f} in {dt0:.5f} + {dt:.5f} s;n: {n}; {dt/n.float*1e9:.3f} ns/eval"
  else:
    when not declared(stdout): import std/[syncio, formatFloat]
    import std/[math, heapqueue]
    proc lnaT*(x: float32): float32 {.inline.} =
      var x = x
      let p = cast[ptr f4s](x.addr)
      p.sign = 0                                  # force x to be positive
      let e = (p.expo.cint - 127).float*LN2       # ln(x*2^y) == ln(x) + y*ln2
      p.expo = 127                                # force x to [1, 2)
      if x > 1.88f32:
        let y = x.float*0.5 - 1.0
        echo "x: ",x," e: ",e," y: ",y
        e + LN2 + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
      elif x < 1.06f32:
        let y = x.float - 1.0
        echo "X: ",x," E: ",e," Y: ",y
        e       + y*(1.0 + y*(-0.5 + y*(1.0/3.0 + y*(-0.25 + y*(0.2 - y/6.0)))))
      else:
        let d = x.float * r1_2                    # x -> dbl [sqrt(1/2), sqrt2)
        let r = (d.float - 1.0)/(d.float + 1.0)   # r for r)atio of -1/+1
        let s = r*r                               # s for s)quare
        echo "x: ",x," d: ",d," e: ",e," r: ",r," s: ",s," iM: ",s.lnaSeries
        float32(e + LNr2 + r*s.lnaSeries)
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
i7_6700k$ for mode in '' -d:fm -d:fim -d:stdlib;{nim c -d:r -d:bench $mode lna>&/n;repeat 3 nor 0 $b ./lna}
S0:5.3e+36 sL:1652640659.073322 in 13.312009 second;n: 8556380160; 1.56 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 13.285249 second;n: 8556380160; 1.55 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 13.296462 second;n: 8556380160; 1.55 ns/eval
                                                                   1.55 +- 0.003
S0:5.3e+36 sL:1652640659.073322 in 10.756718 second;n: 8556380160; 1.26 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 10.712787 second;n: 8556380160; 1.25 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 10.718144 second;n: 8556380160; 1.25 ns/eval
                                                                   1.25 +- 0.003
S0:5.3e+36 sL:1652640659.073322 in 11.040576 second;n: 8556380160; 1.29 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 11.030243 second;n: 8556380160; 1.29 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 10.904071 second;n: 8556380160; 1.27 ns/eval
                                                                   1.27 +- 0.006
S0:5.3e+36 sL:1641011596.122295 in 20.227257 second;n: 8556380160; 2.36 ns/eval
S0:5.3e+36 sL:1641011596.122295 in 20.227509 second;n: 8556380160; 2.36 ns/eval
S0:5.3e+36 sL:1641011596.122295 in 20.231506 second;n: 8556380160; 2.36 ns/eval
                                                                   2.36 +- 0.003
i7_1370P$ for mode in '' -d:fm -d:fim -d:stdlib;{nim c -d:r -d:bench $mode lna>&/n;repeat 3 nor 0 $b ./lna}
S0:5.3e+36 sL:1652640659.073322 in 6.615819 second;n: 8556380160; 0.77 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 6.980933 second;n: 8556380160; 0.82 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 7.320486 second;n: 8556380160; 0.86 ns/eval
                                                                  0.773 +- 0.017
S0:5.3e+36 sL:1652640659.073322 in 7.609612 second;n: 8556380160; 0.89 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 7.661034 second;n: 8556380160; 0.90 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 8.191683 second;n: 8556380160; 0.96 ns/eval
                                                                  0.889 +- 0.003
S0:5.3e+36 sL:1652640659.073322 in 8.293185 second;n: 8556380160; 0.97 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 8.342474 second;n: 8556380160; 0.98 ns/eval
S0:5.3e+36 sL:1652640659.073322 in 8.330391 second;n: 8556380160; 0.97 ns/eval
                                                                  0.969 +- 0.003
S0:5.3e+36 sL:1641011596.122295 in 7.963845 second;n: 8556380160; 0.93 ns/eval
S0:5.3e+36 sL:1641011596.122295 in 8.605017 second;n: 8556380160; 1.01 ns/eval
S0:5.3e+36 sL:1641011596.122295 in 9.766621 second;n: 8556380160; 1.14 ns/eval
                                                                  0.931 +- 0.027
In Summary: Skylake(4.7GHz)      AlderLake (5.2GHzPcore) 2ndBatch           δ
            1.55 +- 0.003        0.773 +- 0.017          (0.773 +- 0.016)  0.0σ
            1.25 +- 0.003        0.889 +- 0.003          (0.881 +- 0.0013) 2.5σ
            1.27 +- 0.006        0.969 +- 0.003          (0.893 +- 0.034)  2.2σ
 1.89x      2.36 +- 0.003 1.20x  0.931 +- 0.027          (0.980 +- 0.01)   1.7σ
Note that assessing CPU superscalar pipeline util is much more subtle than raw
wall clock time.  These "speed-ups" are really ratios of "incremental wall time
per loop per lna() eval" in best possible, hot-everything cases.  Min estimate
here is simply min3 +- (med-min3)/3 which works ok-ish as per final δ. ]#
