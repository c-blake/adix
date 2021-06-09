template cfor*(init, test, update, body: untyped) =
  ## C-like for loop template.  May need () around arguments at call site.
  ## Usage is like: `cfor (var i = 0), i < 16, i += 2: body`
  block:                     # var didOne = false #XXX continue in body broken
    init                     # while true:
    while test:              #   if not test: break
      body                   #   elif didOne: update
      update                 #   else: didOne = true

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
