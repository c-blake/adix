import std/hashes, adix/[bitop, topk]; export topk.TopKOrder
template pua*(T: typedesc): untyped = ptr UncheckedArray[T]
# Since want no `setCap | pairs` to exist for fixed size | set-like `t`, what is
# `concept` is driven by the external interface, but performance tweaks { like
# `isSaved` } instead do `compiles`. Dup concept decls work around concept bugs.

template isSaved(t): untyped =      # S)aved Hashes
  compiles(block:(var vt=t; hash(vt, 0, 0.Hash))) and # Set hash of slot `i`
  compiles(hash(t, 0).Hash)                           # Get hash of slot `i`

#XXX Un-commenting any of `hash(Q)`, `eq`, `keyR`, `key` blocks concept match
type    # More adaptable than Nim std/tables which are VROats in present terms
  Oat*[K,Q] = concept       # Lowest Common Denominator Open-Addressed Table
    proc cap(t: Self): int               # Query alloc num. of slots
#   proc hash(q: Q): Hash                # hash key `k`
#   proc eq(t: Self; a: K, b: K|Q): bool # Stored key `a` == stored|query `b`
    proc keyQ(t: Self, k: K): Q          # Make a query from a stored key
#   proc keyR(t: Self, q: Q): K          # Convert internal Q to ref type K
#   proc key(t: var Self, i: int, k: K)  # Set key for slot `i`
    proc key(t: Self, i: int): K         # Get key for slot `i`
    proc used(t: Self, i: int): bool     # Test if slot `i` is used | free
    proc inUse(c: var Self, n: int)      # Set number of slots in use
    proc inUse(c:     Self): int         # Get number of slots in use

  VOat*[K,Q,V] = concept    # Valued-Oat; Needs val; Adds []/{}/.values/etc.
    proc cap(t: Self): int               # Query alloc num. of slots
#   proc hash(q: Q): Hash                # hash key `k`
#   proc eq(t: Self; a: K, b: K|Q): bool # Stored key `a` == stored|query `b`
    proc keyQ(t: Self, k: K): Q          # Make a query from a stored key
#   proc keyR(t: Self, q: Q): K          # Convert internal Q to ref type K
#   proc key(t: var Self, i: int, k: K)  # Set key for slot `i`
    proc key(t:     Self, i: int): K     # Get key for slot `i`
    proc used(t:    Self, i: int): bool  # Test if slot `i` is used | free
    proc inUse(c: var Self, n: int)      # Set number of slots in use
    proc inUse(c:     Self): int         # Get number of slots in use
    proc val(t: var Self, i: int, v: V)  # Set val for slot `i`
    proc val(t:     Self, i: int): V     # Get val for slot `i`

  ROat*[K,Q] = concept      # R)esizable Oat; Needs new/cp/set; Adds setCap..
    proc cap(t: Self): int               # Query alloc num. of slots
#   proc hash(q: Q): Hash                # hash key `k`
#   proc eq(t: Self; a: K, b: K|Q): bool # Stored key `a` == stored|query `b`
    proc keyQ(t: Self, k: K): Q          # Make a query from a stored key
#   proc keyR(t: Self, q: Q): K          # Convert internal Q to ref type K
#   proc key(t: var Self, i: int, k: K)  # Set key for slot `i`
    proc key(t: Self, i: int): K         # Get key for slot `i`
    proc used(t: Self, i: int): bool     # Test if slot `i` is used | free
    proc inUse(c: var Self, n: int)      # Set number of slots in use
    proc inUse(c:     Self): int         # Get number of slots in use
    proc newOfCap(t: Self, n: int): Self # Get a new `n`-slot instance
    proc copy(t: var Self, i: int, u: Self, j: int) # Abstract t[i] = u[j]
    proc setNew(t, u: var Self)          # Efficiently set all t = u

  VROat*[K,Q,V] = concept   # V)alued, R)esizable Oat; Needs & adds both
    proc cap(t: Self): int               # Query alloc num. of slots
#   proc hash(q: Q): Hash                # hash key `k`
#   proc eq(t: Self; a: K, b: K|Q): bool # Stored key `a` == stored|query `b`
    proc keyQ(t: Self, k: K): Q          # Make a query from a stored key
#   proc keyR(t: Self, q: Q): K          # Convert internal Q to ref type K
#   proc key(t: var Self, i: int, k: K)  # Set key for slot `i`
    proc key(t: Self, i: int): K         # Get key for slot `i`
    proc used(t: Self, i: int): bool     # Test if slot `i` is used | free
    proc inUse(c: var Self, n: int)      # Set number of slots in use
    proc inUse(c:     Self): int         # Get number of slots in use
    proc val(t: var Self, i: int, v: V)  # Set val for slot `i`
    proc val(t:     Self, i: int): V     # Get val for slot `i`
    proc newOfCap(t: Self, n: int): Self # Get a new `n`-slot instance
    proc copy(t: var Self, i: int, u: Self, j: int) # Abstract t[i] = u[j]
    proc setNew(t, u: var Self)          # Efficiently set all t = u

  SomeROat = ROat | VROat
  SomeOat  = SomeROat | Oat | VOat

proc len*(t: SomeOat): int = t.inUse ## stdlib-style slots in-use

iterator probeSeq(h, mask: Hash): int =
  var i: Hash = h and mask                # Start w/home address
  while true: yield i; i = (i+1) and mask # Linear Probing

func oatSlots*(n: int, mnFree=600): int = ceilPow2(n + max(1, mnFree))
  ## Guess of the right table size from expected number of items.

proc oatSlot*[Q](t: SomeOat; q: Q; h: Hash; d: var Hash): int =
  mixin eq      # Q: Set d=0 or just assume like now?
  var j {.noinit.}: int                 # Where to insert if missing
  for i in probeSeq(h, t.cap - 1):
    j = i
    if not t.used i: break              # Need >= 1 FREE slot to halt search
    if (when t.isSaved: t.hash(i) == h else: true) and t.eq(t.key i, q):
      return i
    d.inc       # Q: Also break if d == t.cap?
  -1 - j                                # <0 =>MISSING&insert idx = -1 - result

proc tooFull*(t: SomeOat; d: int; newSize: var int): bool =
  #-> user proc w/some deflt but that BREAKS concept match (barely works as-is).
  let sLen = t.len              # Could be a cap-long loop
  if sLen + 1 + 1 > t.cap:      # Call setCap pre-put? +1 new, +1 free
    newSize = t.cap shl 1; return true
  let p2 = lgCeil(t.cap)        # NOT an over-deep search; Would like to test
  if d < 3*p2 + 1:              #..first since it is guaranteed cheap, but need
    return false                #..cond to ensure tiny tables terminate probeSeq
  if sLen > t.cap shr 1:        # Over-deep on under-full: re-salt hash?
    newSize = t.cap; return true

proc setCap*(t: var SomeROat; newSize = -1) =
  let newSz = if newSize < 0: max(2, t.cap shl 1)
              else: oatSlots(max(newSize, t.len), 1)  # max blocks over-shrink
  if newSz == t.cap and newSize == -1: return
  var ns = t.newOfCap newSz
  var d: int
  for i in 0 ..< t.cap:
    if t.used i:
      let q = t.key i
      let h = when t.isSaved: t.hash(i) else: t.keyQ(q).hash
      ns.copy -1 - ns.oatSlot(q, h, d), t, i
  t.setNew ns

template upSert*(t: SomeOat, q, i, UP, SERT) =
  var d, newSize: Hash
  let h = q.hash
  var i = oatSlot(t, q, h, d)
  if i >= 0: UP
  else:
    if oats.tooFull(t, d, newSize):
      when t is ROat:
        oats.setCap t, newSize; d = 0
        i = oatSlot(t, q, h, d)
      else: raise newException(ValueError, "non-resizable table too full")
    i = -1 - i
    SERT
    when t.isSaved: t.hash i, h     # Late in case `SERT` aborts insert
    t.inUse t.len + 1

proc incl*[K,Q](t: var Oat[K,Q], q: Q) =
  t.upSert(q, i): discard
  do: t.key(i, t.keyR q)

proc `[]`*[K,Q,V](t: VOat[K,Q,V], q: Q): V =
  if (var d: Hash; let i = oatSlot(t, q, q.hash, d); i >= 0): result = t.val i
  else: raise newException(KeyError, "no such key")

proc `[]=`*[K,Q,V](t: var VOat[K,Q,V], q: Q, v: V) =
  t.upSert(q, i): t.val i
  do: t.key(i, t.keyR q); t.val i, v

proc mgetOrPut*[K,Q,V](t: var VOat[K,Q,V], q: Q, v: V): var V =
  t.upSert(q, i): t.val i
  do: t.key(i, t.keyR q); t.val i, v

proc getOrDefault*[K,Q,V](t: VOat[K,Q,V], q: Q): V =
  if (var d: Hash; let i = oatSlot(t, q, q.hash, d); i >= 0): result = t.val i

iterator items*[K,Q](t: Oat[K,Q]): K =
  for i in 0 ..< t.cap: (if t.used i: yield t.key i)

iterator values*[K,Q,V](t: VOat[K,Q,V]): V =
  for i in 0 ..< t.cap: (if t.used i: yield t.val i)

iterator pairs*[K,Q,V](t: VOat[K,Q,V]): (K, V) =
  for i in 0 ..< t.cap: (if t.used i: yield (t.key i, t.val i))

iterator topByVal*[K,Q,V](s: VOat[K,Q,V], n=10, min=V.low, order=topk.Cheap): (K, V)=
  ## Iterate from smallest to largest over biggest `n` items by value in `s`.
  ## If `n==0` this is effectively heapSort of `s` by value `V`.
  var t = initTopK[(V,K)](n)
  for k, v in oats.pairs(s): (if v >= min: t.push (v, k))
  for e in topk.maybeOrdered(t, order): yield (e[1], e[0])

template oatKStack*(s, Self, Cell, off, offT, K, Q) =
  ## Def routines for back-to-back/stacked variable length, unpadded key data.
  proc mem(c: Cell): pointer = s[c.off].addr          # Accessor
  proc key(c: Cell): K = K(mem: c.mem, len: c.len.int) # Accessor #TODO `int`?
  proc keyQ(t: Self, k: K): Q = k
  proc keyR(t: Self, q: Q): K {.used.} = q
  proc eq(t: Self, a: K, b: K|Q): bool =
    when b is K: a == b             # Compare internal as ints for faster resize
    else: t.keyQ(a) == b            # Compare Q bytes with memcmp
  template add(b; k: Q; limit: int; fail): untyped =
    if b.len + k.len <= limit:                        # Ensure addr space
      let off = b.len; b.setLen off + k.len           # Ensure alloc room
      if k.len > 0: copyMem b[off].addr, k.mem, k.len # Maybe copy
      offT(off)                                       # Yield new offset
    else: fail

template oatSeq*(Self, dat) =
  ## Def routines for `seq`-ish `Self`
  proc cap(c: Self): int = c.dat.len
  proc newOfCap(c: Self, n: int): Self = result.dat.setLen n
  proc copy(c: var Self, i: int, d: Self, j: int) = c.dat[i] = d.dat[j]
  proc setNew(c, d: var Self) = swap c.dat, d.dat   # efficient c=d (& d=c)

template oatCounted*(c, Self, cDotPop) =
  ## Def routines for in-use cells counted by Nim-var maybe-ref'd off of a `c`.
  proc inUse(c: var Self, n: int) = cDotPop = typeof(cDotPop)(n) #TODO user grow policy
  proc inUse(c: Self): int {.used.} = cDotPop.int
