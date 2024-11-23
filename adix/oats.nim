import std/hashes, adix/[bitop, topk]; export topk.TopKOrder
type    # More adaptable than Nim std/sets|tables (named ROats|VROats here)
  Oat*[K, Q] = concept t        # Base Concept: Open-Addressed Table
    cap(t) is int                   # Query allocated slot count
    used(t, int) is bool            # Test if slot `i` is used | free
    key(t, int) is K                # Get key/key ref for slot `i`
    keyQ(t, K) is Q                 # Make a Q)uery from a stored K)ey
    keyR(t, Q) is K                 # Convert internal Q to ref type K
    hash(Q) is Hash                 # hash key `k`
    eq(t, K, K) is bool             # Stored key/ref `a` == stored `b`
    eq(t, K, Q) is bool             # Stored key/ref `a` == Query `b`

  Resizable* = concept t        # Adds auto-grow-ability
    newOfCap(t, int) is type(t)     # Get a new `n`-slot instance
    copy(var t, int, t, int)        # Abstract t[i] = u[j]
    setNew(t, var t)                # Efficiently set all t = u

  Valued*[V] = concept t        # Adds dictionary-like interfaces
    val(var t, int, V)              # Set val for slot `i`
    val(t, int) is V                # Get val for slot `i`

  VOat*[K,Q,V] = concept t      # Valued-Oat; Needs val; Adds []/{}/.values/etc.
    t is Valued[V]; t is Oat[K,Q]

  ROat*[K,Q] = concept t        # R)esizable Oat; Needs new/cp/set;Adds setCap..
    t is Resizable; t is Oat[K,Q]

  VROat*[K,Q,V] = concept t     # V)alued, R)esizable Oat; Needs & adds both
    t is Valued[V]; t is ROat[K,Q]

  PutKey*[K] = concept t        # incl,mgetOrPut.. `SERT` puts keys as an atom
    key(var t, int, K)              # Set key for slot `i` (upSert can inline)
  POat*[K,Q] = concept t
    t is PutKey[K]; t is Oat[K,Q]
  VPOat*[K,Q,V] = concept t
    t is PutKey[K]; t is VOat[K,Q]

  Counted* = concept t          # Adds cheap total used slots; else O(N)
    inUse(var t, int)               # Set count of slots in use
    inUse(t) is int                 # Get count of slots in use

  SavedHash* = concept t        # Speed find&more so resize;<64 bits ok if small
    hash(t, 0, Hash)                # Set hash of slot `i`
    hash(t, 0) is Hash              # Get hash of slot `i`

proc len*[K,Q](t: Oat[K,Q]): int = ## stdlib-style slots in-use; Uncounted=>O(N)
  when t is Counted: t.inUse
  else: (for i in 0 ..< t.cap: (if t.used i: inc result))

iterator probeSeq(h, mask: Hash): int =
  var i: Hash = h and mask                # Start w/home address
  while true: yield i; i = (i+1) and mask # Linear Probing

func oatSlots*(n: int, mnFree=600): int = ceilPow2(n + max(1, mnFree))
  ## Guess of the right table size from expected number of items.

proc oatSlot*[K,Q](t: Oat[K,Q]; q: Q; h: Hash; d: var Hash): int =
  mixin eq      # Q: Set d=0 or just assume like now?
  var j {.noinit.}: int                 # Where to insert if missing
  for i in probeSeq(h, t.cap - 1):
    j = i
    if not t.used i: break              # Need >= 1 FREE slot to halt search
    if (when t is SavedHash: t.hash(i) == h else: true) and t.eq(t.key i, q):
      return i
    d.inc       # Q: Also break if d == t.cap?
  -1 - j                                # <0 =>MISSING&insert idx = -1 - result

proc tooFull*[K,Q](t: Oat[K,Q]; d: int; newSize: var int): bool =
  #-> user proc w/some deflt but that BREAKS concept match (barely works as-is).
  let sLen = t.len              # Could be a cap-long loop
  if sLen + 1 + 1 > t.cap:      # Call setCap pre-put? +1 new, +1 free
    newSize = t.cap shl 1; return true
  let p2 = lgCeil(t.cap)        # NOT an over-deep search; Would like to test
  if d < 3*p2 + 1:              #..first since it is guaranteed cheap, but need
    return false                #..cond to ensure tiny tables terminate probeSeq
  if sLen > t.cap shr 1:        # Over-deep on under-full: re-salt hash?
    newSize = t.cap; return true

proc setCap*[K,Q](t: var ROat[K,Q]; newSize = -1) =
  let newSz = if newSize < 0: max(2, t.cap shl 1)
              else: oatSlots(max(newSize, t.len), 1)  # max blocks over-shrink
  if newSz == t.cap and newSize == -1: return
  var ns = t.newOfCap newSz
  var d: int
  for i in 0 ..< t.cap:
    if t.used i:
      let q = t.key i
      let h = when t is SavedHash: t.hash(i) else: t.keyQ(q).hash
      ns.copy -1 - ns.oatSlot(q, h, d), t, i
  t.setNew ns

template upSert*[K,Q](t: var Oat[K,Q], q, i, UP, SERT) =
  var d, newSize: Hash
  let h = q.hash
  var i = oatSlot(t, q, h, d)
  if i >= 0: UP
  else:
    if oats.tooFull(t, d, newSize):
      when t is ROat[K,Q]:
        oats.setCap t, newSize; d = 0
        i = oatSlot(t, q, h, d)
      else: raise newException(ValueError, "non-resizable table too full")
    i = -1 - i
    SERT
    when t is SavedHash: t.hash i, h    # Late in case `SERT` aborts insert
    when t is Counted: t.inUse t.len + 1

proc incl*[K,Q](t: var POat[K,Q], q: Q) =
  t.upSert(q, i): discard
  do: t.key(i, t.keyR q)

proc `[]`*[K,Q,V](t: VOat[K,Q,V], q: Q): V =
  if (var d: Hash; let i = oatSlot(t, q, q.hash, d); i >= 0): result = t.val i
  else: raise newException(KeyError, "no such key")

proc `[]=`*[K,Q,V](t: var VPOat[K,Q,V], q: Q, v: V) =
  t.upSert(q, i): t.val i
  do: t.key(i, t.keyR q); t.val i, v

proc mgetOrPut*[K,Q,V](t: var VPOat[K,Q,V], q: Q, v: V): var V =
  t.upSert(q, i): t.val i
  do: t.key(i, t.keyR q); t.val i, v

proc getOrDefault*[K,Q,V](t: VOat[K,Q,V], q: Q, def=default(V)): V =
  if (var d: Hash; let i = oatSlot(t, q, q.hash, d); i >= 0): result = t.val i

iterator items*[K,Q](t: Oat[K,Q]): K =
  for i in 0 ..< t.cap: (if t.used i: yield t.key i)

iterator values*[K,Q,V](t: VOat[K,Q,V]): V =
  for i in 0 ..< t.cap: (if t.used i: yield t.val i)

iterator pairs*[K,Q,V](t: VOat[K,Q,V]): (K, V) =
  for i in 0 ..< t.cap: (if t.used i: yield (t.key i, t.val i))

iterator topByVal*[K,Q,V](s: VOat[K,Q,V], n=10, min=V.low, order=topk.Cheap): (K, V)=
  ## Yield biggest `n` items by value in `s` in `order`.
  var t = initTopK[(V,K)](n)
  for k, v in oats.pairs(s): (if v >= min: t.push (v, k))
  for e in topk.maybeOrdered(t, order): yield (e[1], e[0])

template oatKStack*(s, Self, Cell, off, offT, K, Q) =
  ## Define routines for back-to-back/stacked var.len, unpadded key data for
  ## `string`-like `s` & `cligen/mslice.MSlice`-like `Q`.
  proc mem(c: Cell): pointer = s[c.off].addr          # Accessor
  proc key(c: Cell): K = K(mem: c.mem, len: c.len.int) # AccessorForUsr-Accessor
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

template oatSeq*(Self, dat) = ## Add routines for `seq`-ish `Self`
  proc cap(c: Self): int = c.dat.len
  proc newOfCap(c: Self, n: int): Self = result.dat.setLen n
  proc copy(c: var Self, i: int, d: Self, j: int) = c.dat[i] = d.dat[j]
  proc setNew(c, d: var Self) = swap c.dat, d.dat   # efficient c=d (& d=c)

template oatCounted*(c, Self, cDotPop) = ## Add inUse for var maybe-ref'd off c.
  proc inUse(c: var Self, n: int) = cDotPop = typeof(cDotPop)(n) #TODO user grow policy
  proc inUse(c: Self): int {.used.} = cDotPop.int
