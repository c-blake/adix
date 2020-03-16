proc pushUp*[T](x: var seq[T], i, n: int) {.inline.} = # move n items up 1
  if n < 1: return                      # moveMem or slice assign may be faster?
  for j in countdown(i + n - 1, i):
    x[j+1] = move x[j]

proc pullDown*[T](x: var seq[T], i, n: int) {.inline.} = # move n items down 1
  if n < 1: return                      # moveMem or slice assign may be faster?
  for j in countup(i, i + n - 1):
    x[j] = move x[j+1]
