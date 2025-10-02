## To paraphrase Mandalorians: These are the ways, meaning various algorithms.
import std/heapqueue

iterator kWayMerge*[T](itrs: openArray[iterator(): T]): T =
  ## k-way merge of ordered itr yields with `std/heapqueue`.
  if itrs.len > 1: 
    type HeapItem = (T, int)
    var hq = initHeapQueue[HeapItem]()
    for i, it in itrs:
      if not it.finished:
        hq.push((it(), i))
    while hq.len > 0:
      let (v, i) = hq.pop()
      yield v
      let it = itrs[i]
      if not it.finished:
        hq.push((it(), i))
  elif itrs.len == 1:
    for v in itrs[0](): yield v
