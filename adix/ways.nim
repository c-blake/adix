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
