when defined(robinHoodMoveMem):
  # This branch gets SEGV and I have not yet tracked down why
  proc pushUp*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items up 1; i.e. ``x[i+1..i+n] = x[i..i+n-1]``
#   if n < 1: return
    moveMem x[i+1].addr, x[i].addr, n * T.sizeof

  proc pullDown*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items down 1; i.e. ``x[i..i+n-1] = x[i+1..i+n]``
#   if n < 1: return
    moveMem x[i].addr, x[i+1].addr, n * T.sizeof
elif defined(robinHoodSlice):
  proc pushUp*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items up 1; i.e. ``x[i+1..i+n] = x[i..i+n-1]``
#   if n < 1: return
    x[i+1 .. i+n] = x[i .. i+n-1]

  proc pullDown*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items down 1; i.e. ``x[i..i+n-1] = x[i+1..i+n]``
#   if n < 1: return
    x[i .. i+n-1] = x[i+1 .. i+n]
else:
  proc pushUp*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items up 1; i.e. ``x[i+1..i+n] = x[i..i+n-1]``
#   if n < 1: return
    for j in countdown(i + n - 1, i):
      x[j+1] = move x[j]

  proc pullDown*[T](x: var seq[T], i, n: int) {.inline.} =
    ## move n items down 1; i.e. ``x[i..i+n-1] = x[i+1..i+n]``
#   if n < 1: return
    for j in countup(i, i + n - 1):
      x[j] = move x[j+1]
