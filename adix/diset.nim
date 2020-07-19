## This module implements a directly indexed set.  The idea is simple - keep an
## ordinary unordered dense sequence of data records but accelerate searching
## with an direct index array ("direct" == of the same size as the key space or
## "alphabet") which tells the index of the record for each (unique) key.  This
## allows any unordered set operation to be done at guaranteed unit cost per
## element: make, insert, delete, member query & iteration with follow-ons to
## optimal union/intersect/etc. all with an arbitrarily "aged" set.  The catch
## is that the "unit" in said cost will only be small if the key space is small
## enough to allocate a direct index and if the working set part of the index
## array fits into "low enough" latency memory.  Otherwise, average-only time
## data structures like hash tables will likely outperform this.  Iteration is
## always in insertion order whenever no deletes have occurred.
##
## The earliest reference I have elaborating the properties of this approach is
## An Efficient Representation for Sparse Sets by Preston Briggs & Linda Torczon
## from Rice in 1993.  It's simple enough that the idea may date back to early
## 1960s DB work (likely by Codd), maybe under a term like "direct indexing".
## The key type here must have an available conversion to ``int``.

import tabkey, althash, setops  #for `getKey` integer extract/convert

type                  # `A` is Element type; Nim leans toward `item` language.
# TODO Here we limit to 256 entries.  To extend while economizing, `idx` can be
# a giant bit vector using an integral number {ceil(lg(range))} of bits / entry
# (dynamically changing entry-size as `range` grows) for =~ n*lg n bits.  This
# can accommodate bigger address spaces than you may have naively thought.
# 24*2**24/8 is only 48 MiB and L3 caches have become that large circa 2020,
# and 17*2**17/8 = 272 KiB <=~ L2 cache while 14*2**14/8 =~ 28 KiB <=~ L1.
# Besides this direct application, this is sort of shared work to optimize the
# Python3.6-like indirect OCSet which has a very similar index structure.
  I = uint8
  DISet*[A] = object  ## Alphabet size determines ``A``
    data: seq[A]
    range: int
    idx: ptr UncheckedArray[I]

proc len*[A](s: DISet[A]): int {.inline.} = s.data.len

proc rawGet[A](s: DISet[A], item: A): int {.inline.} =
  assert(s.range > 0, "Uninitialized DISet")  # Adjust *caller* not here
  let i = int(item.getKey) - item.low.int
  if i >= s.range:
    raise newException(RangeError, "Direct Indexed key limit exceeded")
  let j = s.idx[i].int                  # Get idx, cmp item
  if j < s.len and s.data[j] == item: j else: -1 - i

# Note that duplicate keys are not allowed for this one.
proc rawPut[A](s: var DISet[A], i: int): int {.inline.} =
  s.idx[i] = I(s.len)                   # Save idx  WTF - why not len.I?
  s.data.setLen s.len + 1               # Make room at high index
  return s.len - 1                      # Return slot

proc rawDel[A](s: var DISet[A], i: int) {.inline.} =
  if i == s.len - 1:                    # Avoid depending upon the
    discard s.data.pop                  # ..order of L/R eval below.
  else:
    s.data[i] = s.data.pop              # Move end to idx(del tgt)
    s.idx[s.data[i].getKey] = I(i)      # Update idx(end item)

template getPut(present: untyped, missing: untyped) {.dirty.} =
  if s.range == 0: s.init               # Ensure initialized table
  let i = s.rawGet(item)
  if i < 0:
    let k = s.rawPut(-1 - i)
    missing
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

proc depth[A](s: DISet[A]; item: A): int {.inline.} = 0

##############################################################################
var diNumer*     = 0     ## default numerator (unused)
var diDenom*     = 0     ## default denominator (unused)
var diMinFree*   = 0     ## default min free slots (unused)
var diGrowPow2*  = 0     ## default growth power of 2 (unused)
var diRehash*    = false ## default hcode rehashing behavior (unused)
var diRobinHood* = false ## default to Robin Hood (unused)

proc init*[A](s: var DISet[A], initialSize=0, numer=0, denom=0, minFree=0,
              growPow2=0, rehash=false, robinhood=false) {.inline.} =
  var e: A
  let n = if initialSize != 0: initialSize else: (e.high - e.low).int
  if n != s.range:                      # Never need to zero the index!
    if s.range > 0: s.idx.dealloc
    s.idx = cast[ptr UncheckedArray[I]](alloc n*I.sizeof)
    s.range = n

proc initDISet*[A](initialSize=0, numer=0, denom=0, minFree=0, growPow2=0,
                   rehash=false, robinhood=false): DISet[A] {.inline.} =
  result.init(initialSize)

proc setPolicy*[A](s: var DISet[A], numer=0, denom=0, minFree=0, growPow2=0,
                   rehash=0, robinhood=0) {.inline.} = discard

proc `=destroy`*[A](s: var DISet[A]) {.inline.} =
  if s.range > 0: s.idx.dealloc

proc rightSize*(count: int, numer=0, denom=0, minFree=0): int {.inline,
  deprecated: "Deprecated since 0.2; identity function".} = count

proc getCap*[A](s: var DISet[A]): int {.inline.} = s.range

proc setCap*[A](s: var DISet[A], newSize = -1) = discard

proc contains*[A](s: DISet[A], item: A): bool {.inline.} = s.rawGet(item) >= 0

proc containsOrIncl*[A](s: var DISet[A], item: A): bool {.inline.} =
  getPut do: result = true
  do       : result = false; s.data[k] = item

proc setOrIncl*[A](s: var DISet[A], item: A): bool {.inline.} =
  getPut do: s.data[i] = item; result = true
  do       : s.data[k] = item; result = false

proc mgetOrIncl*[A](s: var DISet[A], item: A, had: var bool): var A =
  getPut do: had = true ; return s.data[i]
  do       : had = false; s.data[k] = item; return s.data[k]

proc add*[A](s: var DISet[A], item: A) {.inline.} =
  raise newException(RangeError, "Direct Indexed sets cannot be multisets")

template withItem*[A](t: var DISet[A], item: A; it, body1, body2: untyped) =
  var i = rawGet(t, item)
  if i >= 0:
    var it {.inject.} = t.data[i].addr
    body1
  else:
    body2

template withItem*[A](t: DISet[A], item: A; it, body1, body2: untyped) =
  var i = rawGet(t, item)
  if i >= 0:
    let it {.inject.} = t.data[i].unsafeAddr
    body1
  else:
    body2

template withItem*[A](t: var DISet[A], item: A; it, body1: untyped) =
  var i = rawGet(t, item)
  if i >= 0:
    var it {.inject.} = t.data[i].addr
    body1

template withItem*[A](t: DISet[A], item: A; it, body1: untyped) =
  var i = rawGet(t, item)
  if i >= 0:
    let it {.inject.} = t.data[i].unsafeAddr
    body1

proc missingOrExcl*[A](s: var DISet[A], item: A): bool =
  popRet do: s.rawDel i
  do       : return true

proc take*[A](s: var DISet[A], item: var A): bool {.inline.} =
  popRet do: item = move(s.data[i]); s.rawDel i; return true
  do: return false

proc pop*[A](s: var DISet[A]): A {.inline.} = s.data.pop

proc clear*[A](s: var DISet[A]) {.inline.} = s.data.setLen 0

iterator items*[A](s: DISet[A]): A =
  for item in s.data: yield item

iterator mitems*[A](s: var DISet[A]): var A =
  for item in s.data: yield item

iterator pairs*[A](s: DISet[A]): (int, A) =
  for i, item in s.data: yield (i, item)

iterator hcodes*[A](s: DISet[A]): tuple[i: int, hc: Hash] =
  for i, item in s.data: yield (i, Hash(item.getKey))

iterator allItems*[A](t: DISet[A]; item: A): A =
  raise newException(RangeError, "Direct Indexed sets cannot be multisets")

proc debugDump*[A](s: DISet[A], label="") =
  if label.len > 0: echo label
  echo s.len, " items"
  echo "data: ", s.data
  echo "index: "
  for i in 0 ..< s.range: echo "  ", s.idx[i]

defSetOps(DISet)  # Define rest of the wide API not dependent upon details
