#TODO Re-do all lptabz.nim ideas here,but in this new-style Concept way.  Done
# well this should be able to back-end: nd/[setFile,invidx], suggest, thes,
# lptabz, likely nimsearch/pack (any persistent|not table sans tombstones.
import std/[hashes, heapqueue], adix/bitop
type
  Void* = distinct array[1, int] ## For V,H,U to disable pairs/hash/use-caching
  Oat*[K,V,H,U] = concept ## optCounted OpenAddrTable of Key,optValues,optHsaved
    proc key(c: Self, i: int): K                  # Cell ops, i-th various
    proc val(c: Self, i: int): V                  # V=Void => no sep val type
    proc used(c: Self, i: int): bool              # Test if slot `i` = used|free
    proc hash(c: var Self, i: int, hc: H)         # H!=Void: set hash for `i`
    proc hash(c: Self, i: int): Hash              # Get hash of slot `i`
    proc copy(c: var Self,i: int, d: Self,j: int) # Abstract c[i] = d[j]
    proc newOfCap(c: Self, n: int): Self          # Get new `n`-slot instance
    proc setNew(c, d: var Self)                   # Efficiently set all c = d
    proc getCap(c: Self): int                     # Query alloc num. of slots
    proc len(c: Self): int                        # stdlib-style slots in-use
    proc nUse(c: var Self, nU: U)                 # U=Void => no-op

iterator probeSeq(hc, mask: Hash): int =
  var i: Hash = hc and mask             # Start w/home address
  while true:
    yield i; i = (i+1) and mask         # Linear Probing

proc rawGet*[K,V,H,U](s: Oat[K,V,H,U]; key: K; hc: Hash; d: var Hash): int =
  var j {.noinit.}: int                 # Where to insert if missing
  for i in probeSeq(hc, s.getCap - 1):
    j = i
    if not s.used(i): break             # Need >=1 FREE slot to terminate
    when H isnot Void:
      if H(s.hash(i)) == H(hc) and s.key(i) == key: return i
    else:
      if s.key(i) == key: return i
    d.inc
  -1 - j                                # <0 =>MISSING&insert idx = -1 - result

func slotsGuess(n: int, mnFree: int): int = ceilPow2(n + max(1, mnFree))

proc tooFull*[K,V,H,U](s: var Oat[K,V,H,U]; d: int; newSize: var int): bool =
  let sLen = s.len                      # A getCap-long loop if unCounted
  if sLen + 1 + 1 > s.getCap:           # Call setCap pre-put? +1 new, +1 free
    newSize = s.getCap shl 1; return true
  let p2 = lgCeil(s.getCap)     # NOT an over-deep search; Would like to test
  if d < 3*p2 + 1:              #..first since it is guaranteed cheap, but need
    return false                #..cond to ensure tiny tables terminate probeSeq
  if sLen > s.getCap shr 1:             # Over-deep on under-full: re-salt hash?
    newSize = s.getCap; return true

proc setCap*[K,V,H,U](s: var Oat[K,V,H,U]; newSize = -1) =
  let newSz = if newSize < 0: max(2, s.getCap shl 1)
              else: slotsGuess(max(newSize, s.len), 1) # max blocks over-shrink
  if newSz == s.getCap and newSize == -1:
    return
  var ns = s.newOfCap(newSz)
  var d: int
  for i in 0 ..< s.getCap:
    if s.used(i): #XXX condition s.hash(i) use on H.sizeof big enough for newSz
      ns.copy(-1 - ns.rawGet(s.key(i), s.hash(i), d), s, i)
  s.setNew ns

template getPut*[K,V,H,U](s: var Oat[K,V,H,U], i, key, hc, present, missing) =
  var d, newSize: Hash
  let hc = hash(key)
  var i = s.rawGet(key, hc, d)
  if i < 0:
    if s.tooFull(d, newSize):
      s.setCap newSize
      i = s.rawGet(key, hc, d)
    i = -1 - i
    s.nUse U(s.len + 1)
    missing
    when H isnot Void: s.hash(i, H(hc))
  else:
    present

proc incl*[K,V,H,U](s: var Oat[K,V,H,U], key: K) =
  when V isnot Void: {.error: "`incl` called when V != Void".}
  s.getPut(i, key, hc): discard
  do: echo "initialize cell ",i #XXX impl

iterator items*[K,V,H,U](s: Oat[K,V,H,U]): K =
  for i in 0 ..< s.getCap: (if s.used(i): yield s.key(i))

iterator pairs*[K,V,H,U](s: Oat[K,V,H,U]): (K, V) =
  when V is Void: {.error: "`pairs` called when V == Void".}
  for i in 0 ..< s.getCap: (if s.used(i): yield (s.key(i), s.val(i)))

iterator topByVal*[K,V,H,U](s: Oat[K,V,H,U], n=10, min=V.low): (K, V) =
  ## Iterate from smallest to largest over biggest `n` items by value in `s`.
  ## If `n==0` this is effectively heapSort of `s` by value `V`.
  when V is Void: {.error: "`topByVal` called when V == Void".}
  proc `<`(a, b: (V,K)): bool = a[0] < b[0] # ignore K => only partial order
  var q = initHeapQueue[(V,K)]()
  for k, v in pairs(s):
    if v >= min:
      let e = (v, k)
      if n == 0 or q.len < n: q.push e
      elif e > q[0]: discard q.replace(e)
  var y: (K,V)
  while q.len > 0:        # q now has top n entries
    let r = q.pop
    y[0] = r[1]
    y[1] = r[0]
    yield y               # yield in ascending order

template keyStack*(s, off, offT, Cell, Key) =
  ## Def routines for back-to-back stacked variable length, unpadded data.
  proc mem(c: Cell): pointer = s[c.off].addr    # Accessors
  proc key(c: Cell): Key =
    result.mem = c.mem; result.len = c.len.int

  proc `==`(a: Key|Cell, b: Cell): bool {.used.} = # Comparator
    a.len == b.len and cmemcmp(a.mem, b.mem, a.len.csize_t) == 0

  template add(b; k: Key; limit: int; fail): untyped =
    if b.len + k.len > limit: fail      # Check address space
    let off = b.len                     # Ensure room
    b.setLen off + k.len
    if k.len > 0: copyMem b[off].addr, k.mem, k.len # Maybe copy
    offT(off)

template useCountedCellSeq*(Cells, dat, nUsed) =
  ## Def routines for `seq`-ish `Cells` with an auxiliary field-ish `nUse`.
  proc copy(c: var Cells, i: int, d: Cells, j: int) = c.dat[i] = d.dat[j]
  proc newOfCap(c: Cells, n: int): Cells = result.dat.setLen n
  proc setNew(c, d: var Cells) = swap c.dat, d.dat  # efficient c = d
  proc getCap(c: Cells): int = c.dat.len
  proc len(c: Cells): int {.used.} = c.nUsed.int    # For concept match
  proc nUse(c: var Cells, nU: int) = c.nUsed = nU

template unCountedCellSeq*(Cells, dat) =
  ## Def routines for `seq`-ish `Cells`
  proc copy(c: var Cells, i: int, d: Cells, j: int) = c.dat[i] = d.dat[j]
  proc newOfCap(c: Cells, n: int): Cells = result.dat.setLen n
  proc setNew(c, d: var Cells) = swap c.dat, d.dat  # efficient c = d
  proc getCap(c: Cells): int = c.dat.len
  proc len(c: Cells): int {.used.} =      # Slower, but space efficient
    for i in 0 ..< c.getCap: result += c.used(i).int
  proc nUse(c: var Cells, nU: int) = discard
