## This module implements a sorted seq representation of a keyed set.  ``cmp``
## must be defined on elements and search is by ``algorithm.upperBound``.
## Duplicate keys are kept in the order in which they are added (i.e. "stable").

import setops, bitop, algorithm, althash
type                    # `A` is Element type; Nim leans toward `item` language.
  SSSet*[A] = object  ## Sorted Seq Set
    data: seq[A]        # data array

proc len*[A](s: SSSet[A]): int {.inline.} = s.data.len

proc rawGet[A](s: SSSet[A]; item: A): int {.inline.} =
  # This returns the LAST of a block of duplicates or else -1 - insertSpot
  if s.data.len == 0: return -1
  let i = s.data.upperBound(item, cmp)  # 1 past LAST of a block of any dups
  let j = max(0, i - 1)
  result = if s.data[j] == item: j else: -1 - i

proc depth[A](s: SSSet[A]; item: A): int {.inline.} = s.data.len.lgCeil

proc rawDel[A](s: var SSSet[A], i: Hash) {.inline.} =
  s.data.delete[i]

template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0: s.init
  var i = s.rawGet(item)
  if i < 0:
    var j = -1 - i
    missing # does s.data.insert e, j
  else:
    present

template popRet(present: untyped, missing: untyped) {.dirty.} =
  if s.data.len == 0:
    missing
  let i = s.rawGet(item)
  if i >= 0:
    present
  else:
    missing

var ssInitialSize* = 4 ## default initial size aka capacity aka cap
var ssNumer*       = 0 ## unused
var ssDenom*       = 0 ## unused
var ssMinFree*     = 0 ## unused
var ssGrowPow2*    = 0 ## unused
var ssRehash*      = false ## unused
var ssRobinHood*   = false ## unused

proc init*[A](s: var SSSet[A], initialSize=0, numer=0, denom=0, minFree=0,
              growPow2=0, rehash=false, robinhood=false) {.inline.} =
  s.data = newSeqOfCap[A](initialSize)

proc initSSSet*[A](initialSize=0, numer=0, denom=0, minFree=0, growPow2=0,
                   rehash=false, robinhood=false): SSSet[A] {.inline.} =
  result.init

proc setPolicy*[A](s: var SSSet[A], numer=0, denom=0, minFree=0, growPow2=0,
                   rehash=false, robinhood=false) {.inline.} =
  ## This is only for API compatibility with other sets and does nothing.
  discard

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc getCap*[A](s: SSSet[A]): int {.inline.} = s.data.len

proc setCap*[A](s: var SSSet[A], newSize = -1) = discard #`seq` has no setCap

proc contains*[A](s: SSSet[A], item: A): bool {.inline.} =
  if s.data.len == 0: return false
  s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var SSSet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data.insert(item, j)

proc setOrIncl*[A](s: var SSSet[A], item: A): bool {.inline.} =
  getPut do: s.data[i] = item; result = true
  do       : s.data.insert(item, j); result = false

proc mgetOrIncl*[A](s: var SSSet[A], item: A, had: var bool): var A {.inline.} =
  getPut do: had = true ; return s.data[i]
  do       : had = false; s.data.insert(item, j); return s.data[j]

proc add*[A](s: var SSSet[A], item: A) {.inline.} =
  if s.data.len == 0: s.init
  s.data.insert(item, s.data.upperBound(item))

template withItem*[A](s: var SSSet[A], itm: A; it, body1: untyped; body2: untyped=nil) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    var it {.inject.} = s.data[i].addr
    body1
  else:
    body2

template withItem*[A](s: SSSet[A], itm: A, it, body1, body2: untyped) =
  mixin rawGet
  let i = s.rawGet(itm)
  if i >= 0:
    let it {.inject.} = s.data[i].unsafeAddr
    body1
  else:
    body2

proc missingOrExcl*[A](s: var SSSet[A], item: A): bool =
  popRet do: s.rawDel i
  do: return true

proc take*[A](s: var SSSet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[i]); s.rawDel i; return true
  do: return false

proc pop*[A](s: var SSSet[A]): var A {.inline.} = s.data.pop

proc clear*[A](s: var SSSet[A]) =
  s.data.setLen 0

iterator items*[A](s: SSSet[A]): A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i]

iterator mitems*[A](s: var SSSet[A]): var A =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i]

iterator pairs*[A](s: SSSet[A]): tuple[a: int, b: A] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield (i, s.data[i])

iterator hcodes*[A](s: SSSet[A]): tuple[i: int, hc: Hash] =
  let L = s.len
  for i in 0 ..< s.data.len:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield (i, hash(s.data[i]))

iterator allItems*[A](s: SSSet[A]; item: A): A =
  let L = s.len
  let i = s.data.upperBound(item, cmp)
  while s.data[i] == item:
    assert(s.len == L, "cannot change a set while iterating over it")
    yield s.data[i]

proc debugDump*[A](s: SSSet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items: ", s.data

defSetOps(SSSet)  # Define rest of the wide API not depending on repr details

proc getByPos*[A](s: SSSet[A], i: int): A {.inline.} = s.data[i]

proc low*[A](s: SSSet[A]): int {.inline.} = s.data.low

proc high*[A](s: SSSet[A]): int {.inline.} = s.data.high

proc lowKey*[A](s: SSSet[A]): A {.inline.} = s.data[0]

proc highKey*[A](s: SSSet[A]): A {.inline.} = s.data[^1]

template withPosItem*[A](s: var SSSet[A], i: int, it, body1: untyped; body2: untyped = nil) =
  if i >= 0 and i < s.data.len:
    var it {.inject.} = s.data[i].addr
    body1
  else:
    body2

template withPosItem*[A](s: SSSet[A], i: int, it, body1, body2: untyped) =
  if i >= 0 and i < s.data.len:
    let it {.inject.} = s.data[i]
    body1
  else:
    body2
