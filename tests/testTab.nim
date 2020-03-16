## This is an `include` file to attach a test suite to a given ??tab.nim impl.

import cligen

proc test*(nums: seq[int32]) =
  var t = initTab()
  for x in nums:
    if x >= 0:
      echo "ADD ", $x
      t.mgetOrPut(x.uint32, 0).inc
    elif (-x).uint32 in t: echo "HAS ", $(-x)
    else: echo "NO ", $(-x)
#   echo t.s.data
  echo t
# echo t.s.data
# echo t.s.depths

dispatch(test)
