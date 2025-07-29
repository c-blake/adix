##[ Top-k of n using "Buffered QuickSelect" for θ(n) scaling in large `k` cases
rather than `std/heapqueue` { which has [O|θ](n*lg k) }. ]##
import std/[random, algorithm]  # quickwit.io/blog/top-k-complexity explains BUT
type                            #..NOTE heap [θO](n*lg k) &this θ(n),O(n*lg k).
  Partn* = enum last, ran       #XXX Tukey's 9th | median-of-medians | PDQ?
  TopK*[T] = object  ## A TopK accumulator; init; push; iterate
    i, k: int
    partn: Partn  # One can do variant/case obj using HeapQueue for small k, but
    first: bool   #..that is only arch-dependent 1.15-1.30X faster for k<100s &
    thr: T        #..saves only 1/2 the space when little is used anyway.  OTOH,
    s: seq[T]     #..a better Partn may make this way "always fastest period".
  TopKOrder* = enum Descending, Ascending, Cheap

proc supportsCopyMem(t: typedesc): bool {.magic: "TypeTrait".}
proc initTopK*[T](k=10, partn=last): TopK[T] =
  ## Initialize a TopK-accumulator for top-`k`; Usage is simply:
  ##
  ## .. code-block:: nim
  ##   var t = initTopK(); for e in 1..99: t.push e
  ##   for e in t: echo e
  result = TopK[T](i: -1, k: k, partn: partn, first: true)
  when supportsCopyMem(T) and declared newSeqUninit:
    result.s = newSeqUninit[T](2*k); result.s.setLen 0

proc qpLast[T](a: var openArray[T]; L, R: int): int =
  let piv = a[R]                        # QuickPartition about last element
  var i = L - 1
  for j in L..<R:                       # a[]<= Ascending, >= Descending
    if a[j] >= piv: inc i; swap a[i], a[j]
  swap a[i + 1], a[R]
  i + 1

proc qpRand[T](a: var openArray[T]; L, R: int): int =
  swap a[rand(L..R)], a[R]              # QuickPartition about random element
  a.qpLast L, R

proc qPart[T](a: var openArray[T]; L, R: int; partn: Partn): int =
  case partn                            # QuickPartition w/various strategies
  of last: qpLast a, L, R
  of ran : qpRand a, L, R

proc pqs[T](a: var openArray[T]; k: int, partn: Partn): T = # Partial QuickSort
  var (L, R) = (a.low, a.high)          # Returns pivot element
  while L <= R: # Partition a[L..R] about piv; Find its pos
    let pivIx = qPart(a, L, R, partn)
    if pivIx == k - 1: return a[pivIx]  # piv itself is k-th
    elif pivIx > k - 1: R = pivIx - 1   # k-th on left
    else              : L = pivIx + 1   # k-th on right

proc push*[T](t: var TopK[T], e: sink T) =
  ## Incorporate element `e` into `t` for eventual exact `for e in t: ..`.
  inc t.i
  if t.i < t.k: t.s.add e               # Build phase: Always Add
  elif t.first or e > t.thr:            # Filter into batches
    t.s.add e                           # Add if building | over threshold
    if t.s.len == 2*t.k:
      t.thr = t.s.pqs(t.k, t.partn)     # Median -> new thr & all >= put on left
      t.first = false                   # Mark thr active
      t.s.setLen t.k                    # Drop < new median half

proc saw*[T](t: TopK[T]): int = t.i + 1 ## `push` count since last init|clear

iterator items*[T](t: var TopK[T]): lent T =
  ## iterate over `t` yielding top items in cheapest/system order.
  if t.saw > t.k:
    t.thr = t.s.pqs(t.k, t.partn)       # Median -> new thr & all >= put on left
    t.first = false                     # Mark thr active
    t.s.setLen t.k                      # Drop < new median half
  for e in t.s: yield e

iterator descending*[T](t: var TopK[T]): lent T =
  ## iterate over `t` yielding top items in DESCENDING order.
  t.s.sort(order=SortOrder.Descending); t.s.setLen min(t.k, t.saw)
  for e in t.s: yield e

iterator ascending*[T](t: var TopK[T]): lent T =
  ## iterate over `t` yielding top items in ASCENDING order.
  t.s.sort(order=SortOrder.Descending); t.s.setLen min(t.k, t.saw)
  t.s.reverse
  for e in t.s: yield e

iterator maybeOrdered*[T](t: var TopK[T], order=topk.Cheap): lent T =
  ## iterate over `t` yielding top items in various orders.
  case order
  of topk.Cheap     : (for e in topk.items(t)     : yield e)
  of topk.Ascending : (for e in topk.ascending(t) : yield e)
  of topk.Descending: (for e in topk.descending(t): yield e)

proc clear*[T](t: var TopK[T]) = ## Reset `TopK` accumulator
  t.i = -1; t.s.setLen 0; t.first = true

when isMainModule: # Good check: nim r -d:ck -d:r topk -qk3 -n10 -t3628800 #10!
  when not declared stderr: import std/syncio
  import cligen, std/[times, sugar, math, sets]
  when defined danger: randomize()
  proc top(k=500, n=50000, trials=50, partn=last, quiet=false) =
    let tScl = 1e12/n.float/log2(k.float) # picosec/work-scale
    var t = initTopK[int](k, partn)
    var a = collect(for i in 0..<n: i)
    when defined ck: (let ans = collect(for i in max(0, n - k) ..< n: {i}))
    for tr in 1..trials:
      a.shuffle; t.clear
      let t0 = epochTime()
      for e in a: t.push e
      if not quiet: echo int((epochTime() - t0)*tScl)
      let res = collect(for e in t: {e})
      when defined ck: (if res != ans: stderr.write "!!: ",res," != ",ans,"\n")
      else: discard res                 # Suppress unused warning
  dispatch top
