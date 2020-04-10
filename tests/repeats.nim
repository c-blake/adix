import tables, cligen

proc repeats(thresh=2) =
  ## Read 8 byte hashes from stdin & print histogram of any with count > thresh.
  var h: int64
  var cnt: Table[int64, int]
  while stdin.readBuffer(cast[cstring](h.addr), 8) == 8:
    cnt.mgetOrPut(h, 0).inc
  for h,c in cnt:
    if c >= thresh:
      echo "h: ", h, " count: ", c

dispatch(repeats)
