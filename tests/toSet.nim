import cligen, adix/lptabz

proc test(nums: seq[int32]) =
  when defined(fromVar):
    var s: LPSet[int32]
    s.setCap(nums.len)
  else:
    var s = initLPSet[int32](nums.len, minFree=0)
  for x in nums: s.incl x
  echo s.getCap
  echo s

dispatch(test)
