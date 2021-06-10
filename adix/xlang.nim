template cfor*(init, test, update, body: untyped) =
  ## C-like for loop template.  May need () around arguments at call site.
  ## Usage is like: `cfor (var i = 0), i < 16, i += 2: body`
  block:
    init                    #NOTE: Simpler `init; while test: body; update`
    var updated = false     #      is a leaky abstraction; User must know to
    while true:             #      not use `update`-skipping `continue`.
      if updated: update    # Skip first update to be "top test; bottom update"
      else: updated = true
      if test: body
      else: break

proc `&=`*[T, U](a: var T, b: U) {.inline.} =
  ## Updating bit-wise `and`
  a = a and b

proc `|=`*[T, U](a: var T, b: U) {.inline.} =
  ## Updating bit-wise `or`
  a = a or b

proc `^=`*[T, U](a: var T, b: U) {.inline.} =
  ## Updating bit-wise `xor`
  a = a xor b

proc `<<=`*[T, U](a: var T, b: U) {.inline.} =
  ## Updating bit-wise `shl`
  a = a shl b

proc `>>=`*[T, U](a: var T, b: U) {.inline.} =
  ## Updating bit-wise `shr`
  a = a shr b
