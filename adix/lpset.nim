import lptabz

type
  LPSet*[K,V] = LPTabz[K,void,void,0] ## LPTabz specialized to no-sentinel sets

proc initLPSet*[K](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                   minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood): LPSet[K] {.inline.} =
  ## Return an LPTabz specialized to non-sentinel sets.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
