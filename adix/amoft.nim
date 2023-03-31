## `Approximately k-Most Oft <en.wikipedia.org/wiki/Count%E2%80%93min_sketch>`_.
## This is constant space & constant inc&query time with adjustably small error.
## `AMOft[K,C]` augments the sketch with an O(k) paired Table & HeapQueue.
## Present impl does NOT scale well to very large `k` (past ~top-1000).  E.g.:
##
## .. code-block:: nim
##   var amo = initAMOft[int, uint32](k=10)
##   for i in 0..<500: amo.inc i, i # Not v.skewed => not v.accurate
##   for (i, c) in amo.mostCommon(5): echo i, " ", c

import std/[hashes, heapqueue, tables, algorithm]
type
  CtMnSketch*[K,C] = object ## CountMinSketch over hashable `K` & counters `C`.
    data: seq[seq[C]]
    salts: seq[Hash]
    w: int

  AMOft*[K,C] = object ## Track most often hashable `K` with counters `C`.
    sketch: CtMnSketch[K,C]     # Sketch for gigantic `K` spaces
    top: HeapQueue[(C,int)]     # Most frequent counts, keys
    no2key: seq[K]              # For expensive K, like string, the heap ops..
    key2no: Table[K, int]       #..& scan are much faster with Indirect K.
    k: int                      # Num most often keys to track (max `top` len)

proc initCtMnSketch*[K,C](w: int, d=4, salts: seq[int] = @[]): CtMnSketch[K,C] =
  ## `w`=Size(count tables), larger implies less overestimation.  `d`=nTables,
  ## larger implies less P(overEst). `salts`=Override default generated salts.
  if w <= 0 or d <= 0:
    raise newException(ValueError, "Table size(`w`) & hashes(`d`) must be > 0")
  result.w = w
  if salts.len>0: result.salts = salts
  else          : result.salts.setLen d
  result.data.setLen result.salts.len
  for i, t in result.data.mpairs:
    t.setLen w
    result.salts[i] = hash(if salts.len>0: salts[i] else: cast[int](t[i].addr))

proc inc*[K,C](cs: var CtMnSketch[K,C], key: K, r=1): C {.discardable.} =
  ## Count `key` `r` times; Gives new counts; I.e., `(key, r=0)` can query.
  result = C.high
  let kh = hash(key)
  when defined(cmsOnePass):             # This updates faster/more independently
    for i, s in cs.salts:               #..BUT has less accurate estimates.
      let h = Hash(uint(!$(kh !& s)) mod cs.w.uint)
      cs.data[i][h] += r.C
      result = min(result, cs.data[i][h])
  else:
    var old = C.high
    var hs: array[32, Hash] # Avoid inner loop alloc w/bound; 128-256B of stack
    for i, s in cs.salts:
      hs[i] = Hash(uint(!$(kh !& s)) mod cs.w.uint)
      old = min(old, cs.data[i][hs[i]])
    old += r.C
    for i in 0 ..< cs.salts.len:
      cs.data[i][hs[i]] = max(cs.data[i][hs[i]], old)
      result = min(result, cs.data[i][hs[i]])

proc initAMOft*[K,C](k, w: int; d=4, salts: seq[int] = @[]): AMOft[K,C] =
  result.sketch = initCtMnSketch[K,C](w,d,salts); result.k = k

proc slot[K,C](a: var AMOft[K,C], kn: int): int =
  result = -1                           # Must search only by key since updates
  for i in 0 ..< a.top.len:             #..of OTHER keys MAY bump old estimates.
    if a.top[i][1] == kn:               # Also cannot idx with Table since each
      return i                          #..replace re-orders heap slots.  Links?
# While above is a linear scan, Table look-up ensures it only happens if needed.

proc inc*[K,C](a: var AMOft[K,C], key: K, r=1) =
  ## Count `key` as being seen `r` times.
  let c = a.sketch.inc(key, r)          # Form (sketch count, keyNo) 2-tuple
  var new = (c, a.key2no.getOrDefault(key, -1))
  if c.int > r and new[1] != -1 and (let i = a.slot(new[1]); i >= 0):
    a.top.del i                         # Hit: update existing; O(k+lg k fix-up)
    a.top.push new                      # O(lg k)
  elif a.top.len < a.k:                 # Miss/initial build
    new[1] = a.no2key.len
    a.no2key.add key
    a.key2no[key] = new[1]
    a.top.push new
  elif new > a.top[0]:                  # Miss/frequent enough to bump old top:
    new[1] = a.top[0][1]
    discard a.top.replace(new)          # pop min, push new, discard old
    a.key2no.del a.no2key[new[1]]
    a.key2no[key] = new[1]
    a.no2key[new[1]] = key

iterator mostCommon*[K,C](a: AMOft[K,C], k=0): (K, C) =
  ## Yield (`k`-most common values in `a`, each count) tuples; `k<=0` => known.
  let k   = if k > 0: k else: a.top.len
  var cpy = a.top                       # A tree top can duck both O(N) copy..
  var res: seq[(C, int)]                #..to not edit & need to collect for top
  while cpy.len > 0: res.add cpy.pop
  res.sort
  var v: (K, C)
  for i in res.len - k ..< res.len:
    v[0] = a.no2key[res[i][1]]; v[1] = res[i][0]
    yield v

when isMainModule:                      # Check/demo CtMnSketch[K,C], AMOft[K,C]
  when not declared(assert): import std/assertions
  var c = initCtMnSketch[int, uint32](w=16, salts = @[1,2,3])
  for i in 0..<16: c.inc i, i
  for i in 0..<16:
    try: assert c.inc(i, 0) == i.uint32 # |exclusions also work for cmsOnePass
    except AssertionDefect: (if i notin [6,9]: echo "mismatch at ", i)

  var a = initAMOft[string, uint8](k=4, w=16, salts = @[1,2,3,4,5,6,7])
  for i in 0..<32: a.inc $i, 32-i
  var res = ""
  for (i, c) in a.mostCommon(3): res.add i
  assert res == "210"                   # Top 3

  var oft = initAMOft[int, uint32](k=50, w=8192, d=7) # Linear dist not v.skewed
  for i in 0..<50000: oft.inc i, i # 50000*49999/2 = 1_249_975_000 virt.events
  for (i, c) in oft.mostCommon(25): echo i, " ", c, " ", c.int - i,"/1249975000"
