import lptabz

type
  LPSetz*[K,Z;z: static[int]] = LPTabz[K,void,Z,z] ## LPTabz specialized to sentinel sets

proc initLPSetz*[K,Z;z: static[int]](initialSize=lpInitialSize, numer=lpNumer,
                                     denom=lpDenom, minFree=lpMinFree,
                                     growPow2=lpGrowPow2, rehash=lpRehash,
                                     robinhood=lpRobinHood): LPSetz[K,Z;z] {.inline.} =
  ## Return an LPTabz specialized to sets with a sentinel key.
  ## See initLPTabz for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
