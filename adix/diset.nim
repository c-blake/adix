import ditab

type
  DISet*[K,V] = DITab[K,void] ## DITab specialized to sets

proc initDISet*[K](initialSize=0, numer=diNumer, denom=diDenom,
                   minFree=diMinFree, growPow2=diGrowPow2, rehash=diRehash,
                   robinhood=diRobinHood): DISet[K] {.inline.} =
  ## Return an DITab specialized to sets operations.
  ## See initDITab for parameter details.
  result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood
