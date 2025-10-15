## To paraphrase Mandalorians: "These are the ways" (various algorithms).
import std/heapqueue

iterator kWayMerge*[T](itrs: openArray[iterator(): T]): T =
  ## k-way merge of ordered `itrs[i]` yields using `std/heapqueue`.
  if itrs.len > 1:
    type HeapItem = (T, int)
    var hq = initHeapQueue[HeapItem]()
    for i, it in itrs:      # Load min-heap with the first yield of each.
      let vNext = it()      # Must call for system to know exhaustion
      if not it.finished:   #..but want if-guard before push to avoid
        hq.push (vNext, i)  #..having exhausted iterators in the heap.
    while hq.len > 0:       # While heap is not empty:
      let (v, i) = hq.pop   #   get & yield the next min.
      yield v
      let it = itrs[i]      #   push next item from just yielded..
      let vNext = it()
      if not it.finished:   #  ..(unless it's exhausted)
        hq.push (vNext, i)
  elif itrs.len == 1:       # special case of only 1 (elif=>or 0) iter
    for v in itrs[0](): yield v

when isMainModule:
  iterator i0: int {.closure.} = discard
  iterator i1: int {.closure.} = yield 3
  iterator i2: int {.closure.} = yield 1; yield 5
  iterator i3: int {.closure.} = yield 2; yield 4; yield 6
  for i in [i0, i1, i2, i3].kWayMerge: echo i

iterator succPairs*[T](src: iterator:T; stride=1): (T, T) =
  ## Yield successive pairs (src[i - stride], src[i]) for all valid i
  var counter = stride  # Whether to act|wait
  var it0: T            # Running reference value
  var haveOne = false   # Flag indicating we have above
  for it in src():
    if haveOne:         # We are waiting
      dec counter
      if counter == 0:  # Waited right amount
        yield (it0, it)
        it0 = it
        counter = stride
    else:               # Transition -> waiting
      it0 = it          #..with a held reference val
      haveOne = true

iterator diffs*[T](src: iterator:T; stride=1): T =
  ## First differences
  for x0, x in succPairs(src, stride):
    yield x - x0

iterator diffs2*[T](src: iterator:T; stride=1): T =
  ## Second differences
  proc diffs1: iterator: T =
    iterator: T =
      for it in diffs(src): yield it
  for x in diffs(diffs1(), stride): yield x

iterator ratios*[T](src: iterator:T; stride=1): T =
  ## First ratios
  for x0, x in succPairs(src, stride):
    yield x/x0 # guard w/if x0 != 0?

iterator returns*[T](src: iterator:T; stride=1): T =
  ## Arithmetic returns (of, e.g. prices)
  for x0, x in succPairs(src, stride):
    yield x/x0 - 1

proc seqItems[T](src: seq[T]): iterator:T = # Cannot be openArray since..
  iterator:T = (for x in src: yield x)      #..that can live on the stack.

proc diffs*[T](src: seq[T]; stride=1): seq[T] =
  ## Batch first differences of random-access `src` (vectorizable).
# when T is uint8: ..   # To vectorize must at least..
# when T is float32: .. #..fan-out based on `T`.
  for d in diffs(src.seqItems, stride): result.add d # slow for now

when isMainModule:
  iterator nums: float {.closure.} = (for i in 1..9: yield i.float)
  for d in diffs(nums): echo d   # 1 yields 2-1, 3-2, 4-3, .. = 9 1s
  for d in diffs2(nums): echo d  # 2 yields 1-1, 1-1, 1-1, .. = 8 0s
  for r in ratios(nums): echo r  # 3 yields 2/1,3/2,4/3, ..
  for r in returns(nums): echo r # 4 yields 2/1-1,3/2-1,4/3-1, ..
  let x = [1, 2, 3, 4, 5, 6, 7, 8, 9]; echo diffs(@x) # AOT 1
