import lptabz

type
  LPTab*[K,V] = LPTabz[K,V,void,0] ## LPTabz specialized to no-sentinel tables

proc initLPTab*[K,V](initialSize=lpInitialSize, numer=lpNumer, denom=lpDenom,
                     minFree=lpMinFree, growPow2=lpGrowPow2, rehash=lpRehash,
                     robinhood=lpRobinHood): LPTab[K,V] {.inline.} =
  ## Return an LPTabz specialized to tables with no sentinel key.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
