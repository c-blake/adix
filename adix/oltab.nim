import oltabzo

type
  OLTab*[K,V] = OLTabzo[K,V,void,0,8] ## OLTabZO specialized to no-sentinel,8-bit-hcode tables

proc initLPTab*[K,V](initialSize=olInitialSize, numer=olNumer, denom=olDenom,
                     minFree=olMinFree, growPow2=olGrowPow2, rehash=olRehash,
                     robinhood=olRobinHood): OLTab[K,V] {.inline.} =
  ## Return an OLTabzo specialized to tables with no sentinel key.
  ## See initOLTabzo for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
