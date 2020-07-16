#NOTE: When caller just declares var x: Tab, we cannot control initialization :(
when defined(stdlibTab):
  import tables, sets
  type Tab*[K,V] = Table[K,V]
  type Set*[A] = HashSet[A]
  proc initTab*[K,V](sz=4, numer=1, denom=1, minFree=1, growPow2=1,
                     rehash=false, robinhood=false): Tab[K,V] {.inline.} =
    initTable[K,V](sz)
  proc initSet*[A](sz=4, numer=1, denom=1, minFree=1, growPow2=1, rehash=false,
                   robinhood=false): Set[A] {.inline.} =
    initHashSet[A](sz)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export tables, sets
elif defined(tombstone):
  import adix/[tstab, tsset]
  type Tab*[K,V] = TSTab[K,V]
  type Set*[A] = TSSet[A]
  proc initTab*[K,V](sz=4, numer=tsNumer, denom=tsDenom, minFree=tsMinFree,
      growPow2=tsGrowPow2, rehash=tsRehash, robinhood=false):Tab[K,V]{.inline.}=
    initTSTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=tsNumer, denom=tsDenom, minFree=tsMinFree,
      growPow2=tsGrowPow2, rehash=tsRehash, robinhood=false): Set[A] {.inline.}=
    initTSSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export tstab, tsset
elif defined(orderedTombstone):
  import adix/[ottab, otset]
  type Tab*[K,V] = OTTab[K,V]
  type Set*[A] = OTSet[A]
  proc initTab*[K,V](sz=4, numer=otNumer, denom=otDenom, minFree=otMinFree,
      growPow2=otGrowPow2, rehash=otRehash, robinhood=false):Tab[K,V]{.inline.}=
    initOTTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=otNumer, denom=otDenom, minFree=otMinFree,
      growPow2=otGrowPow2, rehash=otRehash, robinhood=false): Set[A] {.inline.}=
    initOTSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export ottab, otset
elif defined(orderedLinear):
  import adix/[oltab, olset]
  type Tab*[K,V] = OLTab[K,V]
  type Set*[A] = OLSet[A]
  proc initTab*[K,V](sz=4, numer=olNumer, denom=olDenom, minFree=olMinFree,
      growPow2=olGrowPow2, rehash=olRehash, robinhood=olRobinHood):Tab[K,V]{.inline.}=
    initOLTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=olNumer, denom=olDenom, minFree=olMinFree,
      growPow2=olGrowPow2, rehash=olRehash, robinhood=olRobinHood): Set[A] {.inline.}=
    initOLSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export oltab, olset
elif defined(noRehash):
  import adix/[lptab, lpset]
  type Tab*[K,V] = LPTab[K,V]
  type Set*[A] = LPSet[A]
  proc initTab*[K,V](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
      growPow2=lpGrowPow2, rehash=false, robinhood=olRobinHood):Tab[K,V]{.inline.}=
    initLPTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
      growPow2=lpGrowPow2, rehash=false, robinhood=olRobinHood): Set[A] {.inline.}=
    initLPSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export lptab, lpset
elif defined(noRobinHood):
  import adix/[lptab, lpset]
  type Tab*[K,V] = LPTab[K,V]
  type Set*[A] = LPSet[A]
  proc initTab*[K,V](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
      growPow2=lpGrowPow2, rehash=lpRehash, robinhood=false):Tab[K,V]{.inline.}=
    initLPTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
      growPow2=lpGrowPow2, rehash=lpRehash, robinhood=false): Set[A] {.inline.}=
    initLPSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export lptab, lpset
elif defined(directIndex):
  import adix/[ditab, diset]
  type Tab*[K,V] = DITab[K,V]
  type Set*[A] = DISet[A]
  proc initTab*[K,V](sz=0, numer=diNumer, denom=diDenom, minFree=diMinFree,
      growPow2=diGrowPow2, rehash=diRehash, robinhood=false):Tab[K,V]{.inline.}=
    initDITab[K,V](0, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=0, numer=diNumer, denom=diDenom, minFree=diMinFree,
      growPow2=diGrowPow2, rehash=diRehash, robinhood=false): Set[A] {.inline.}=
    initDISet[A](0, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export ditab, diset
elif defined(integerTab0):
  import adix/[iltab, ilset]
  type Tab*[K,V] = ILTab[K,V,0]
  type Set*[A] = ILSet[A,0]
  proc initTab*[K,V](sz=4, numer=ilNumer, denom=ilDenom, minFree=ilMinFree,
      growPow2=ilGrowPow2, rehash=ilRehash, robinhood=ilRobinHood):Tab[K,V]{.inline.}=
    initILTab[K,V,0](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=ilNumer, denom=ilDenom, minFree=ilMinFree,
      growPow2=ilGrowPow2, rehash=ilRehash, robinhood=ilRobinHood): Set[A] {.inline.}=
    initILSet[A,0](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export iltab, ilset
elif defined(integerTabM1):
  import adix/[iltab, ilset]
  type Tab*[K,V] = ILTab[K,V,-1]
  type Set*[A] = ILSet[A,-1]
  proc initTab*[K,V](sz=4, numer=ilNumer, denom=ilDenom, minFree=ilMinFree,
      growPow2=ilGrowPow2, rehash=ilRehash, robinhood=ilRobinHood):Tab[K,V]{.inline.}=
    initILTab[K,V,-1](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=ilNumer, denom=ilDenom, minFree=ilMinFree,
      growPow2=ilGrowPow2, rehash=ilRehash, robinhood=ilRobinHood): Set[A] {.inline.}=
    initILSet[A,-1](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export iltab, ilset
else:
  import adix/["lptab", "lpset"]
  type Tab*[K,V] = LPTab[K,V]
  type Set*[A] = LPSet[A]
  proc initTab*[K,V](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
                     growPow2=lpGrowPow2, rehash=lpRehash,
                     robinhood=lpRobinHood): Tab[K,V] {.inline.} =
    initLPTab[K,V](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc initSet*[A](sz=4, numer=lpNumer, denom=lpDenom, minFree=lpMinFree,
                   growPow2=lpGrowPow2, rehash=lpRehash,
                   robinhood=lpRobinHood): Set[A] {.inline.} =
    initLPSet[A](sz, numer, denom, minFree, growPow2, rehash, robinhood)
  proc rightSz*(x: Natural): int {.inline,deprecated: "identity; do not use".}=x
  export lptab, lpset
