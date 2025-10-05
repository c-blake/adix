## To paraphrase Mandalorians: "These are the ways" (various algorithms).
import std/heapqueue

iterator kWayMerge*[T](itrs: openArray[iterator(): T]): T =
  ## k-way merge of ordered `itrs[i]` yields using `std/heapqueue`.
  if itrs.len > 1: 
    type HeapItem = (T, int)
    var hq = initHeapQueue[HeapItem]()
    for i, it in itrs:
      let vNext = it()      # Must call for system to know exhaustion
      if not it.finished:   #..but want if-guard before push to avoid
        hq.push (vNext, i)  #..having exhausted iterators in the heap.
    while hq.len > 0:
      let (v, i) = hq.pop
      yield v
      let it = itrs[i]
      let vNext = it()
      if not it.finished:
        hq.push (vNext, i)
  elif itrs.len == 1:
    for v in itrs[0](): yield v

when isMainModule:
  iterator i0: int {.closure.} = discard
  iterator i1: int {.closure.} = yield 3
  iterator i2: int {.closure.} = yield 1; yield 5
  iterator i3: int {.closure.} = yield 2; yield 4; yield 6
  for i in [i1, i2, i3].kWayMerge: echo i
