when not declared(addFloat): import std/formatfloat
import random, stats, adix/[metab, althash, bitop], cligen

proc statsVratio(N=7000, MaxNum=(1 shl 32), numer=1, denom=1,
                 rehash=true, Robin=true, trials=10) =
  randomize()
  var util, dmax, dmean, dvar: RunningStat
  for t in 1..trials:
    var set = initSet[int](numer=numer, denom=denom,
                           rehash=rehash, robinhood=Robin)
    for i in 1..N:
      let c0 = set.getCap
      let ut = set.len.float / set.getCap.float
      let ds = set.depthStats
      set.incl rand(MaxNum)
      if set.getCap != c0 and c0 > 64:  # resized
        util.push  ut
        dmean.push ds[0]
        dvar.push  ds[1]
        dmax.push  ds[2].float / lg(set.getCap).float

  echo "ratio: " , numer, "/", denom, " util: " , util.mean,
       " dmean: ", dmean.mean, " dvar: " , dvar.mean, " dmax/lg: " , dmax.mean

dispatch(statsVratio)
