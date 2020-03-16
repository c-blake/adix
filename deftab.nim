# These operations need know almost nothing about internal set representations.
# They all have the same signatures & semantics of stdlib.tables.  This layering
# occasionally means forming a Pair[K,V] tuple with a dummy value.  C++ STL also
# does this.  Forming but discarding such work ought to be a highly recognized
# pattern any optimizing backend compiler can skip, though.  The only weird
# wrinkle is that init/setPolicy procs have a "union" interface for run-time
# parameters across all impls.  E.g., they must all accept `robinhood`, but can
# ignore any irrelevant params.

import algorithm        # TODO template/mac to make ref variants for sets & tabs
export SortOrder        # TODO fatten interface for diset

template defTab*(T: untyped, S: untyped, G: untyped) =
  export Pair, hash, `==`, cmpKey, cmpVal, high, low, getKey, # keys
         rightSize, items  # WTF `mixin` does not work in generic iterators?

  type T*[K,V] = object ## KV-wrapper over set reprs amenable to satellite data
    s: S[Pair[K,V]]

  proc init*[K,V](t:var T[K,V], initialSize=4, numer=`G Numer`, denom=`G Denom`,
                  minFree=`G MinFree`, growPow2=`G GrowPow2`, rehash=`G Rehash`,
                  robinhood=`G RobinHood`) {.inline.} =
    t.s.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

  proc `init T`*[K,V](initialSize=4, numer=`G Numer`, denom=`G Denom`,
      minFree=`G MinFree`,growPow2=`G GrowPow2`,rehash=`G Rehash`,
      robinhood=`G RobinHood`): T[K,V] {.inline.} =
    result.init initialSize, numer, denom, minFree, growPow2, rehash, robinhood

  proc setPolicy*[K,V](t: var T[K,V], numer=`G Numer`, denom=`G Denom`,
                       minFree=`G MinFree`, growPow2=`G GrowPow2`,
                       rehash=`G Rehash`, robinhood=`G RobinHood`) {.inline.} =
    t.s.setPolicy(numer, denom, minFree, growPow2, rehash, robinhood)

  proc depths*[K,V](t: T[K,V]): seq[int] = t.s.depths

  proc depthStats*[K,V](t: T[K,V]): tuple[m1,m2: float;mx: int] = t.s.depthStats

  proc debugDump*[K,V](t: T[K,V], label="") = t.s.debugDump(label)

  proc len*[K,V](t: T[K,V]): int {.inline.} = t.s.len

  proc getCap*[K,V](t: var T[K,V]): int {.inline.} = t.s.getCap

  proc setCap*[K,V](t: var T[K,V], newSize = -1) = t.s.setCap(newSize)

  proc `to T`*[K,V](pairs: openArray[(K,V)]): T[K,V] =
    result.init rightSize(pairs.len)
    for key, val in pairs: result.add(key, val)  # clobber w/result[key] = val?

  proc mgetOrPut*[K,V](t: var T[K,V], key: K, val: V): var V =
    var had: bool
    t.s.mgetOrIncl((key, val), had).val

  proc mgetOrPut*[K,V](t: var T[K,V], key: K, val: V, had: var bool): var V =
    t.s.mgetOrIncl((key, val), had).val

  proc contains*[K,V](t: T[K,V], key: K): bool {.inline.} =
    (key, default(V)) in t.s

  template withValue*[K,V](t: var T[K,V], key: K, v, body: untyped) =
    mixin withItem
    var vl: V
    let itm: Pair[K,V] = (key, vl)
    t.s.withItem(itm, it) do:
      var v {.inject.} = it.val.addr
      body
 
  template withValue*[K,V](t: var T[K,V], key: K; v, body1, body2: untyped) =
    mixin withItem
    var vl: V
    let itm: Pair[K,V] = (key, vl)
    t.s.withItem(itm, it) do:
      var v {.inject.} = it.val.addr
      body1
    do: body2

  proc raiseNotFound[K](key: K) =
    when compiles($key):
      raise newException(KeyError, "key not found: " & $key)
    else:
      raise newException(KeyError, "key not found")

  proc `[]`*[K,V](t: T[K,V], key: K): V {.inline.} =
    mixin withValue
    t.withValue(key, value) do: return value[]
    do: raiseNotFound(key)

  proc `[]`*[K,V](t: var T[K,V], key: K): var V {.inline.} =
    mixin withValue
    t.withValue(key, value) do: return value[] #XXX check caller gets table val
    do: raiseNotFound(key)

  proc `[]=`*[K,V](t: var T[K,V], key: K, val: V) =
    discard t.s.setOrIncl((key, val))   # Replace FIRST FOUND item in multimap

  proc hasKey*[K,V](t: T[K,V], key: K): bool {.inline.} =
    t.withValue(key, it) do: result = true

  proc hasKeyOrPut*[K,V](t: var T[K,V], key: K, val: V): bool {.inline.} =
    discard t.s.mgetOrIncl((key, val), result)

  proc getOrDefault*[K,V](t: T[K,V], key: K, default=default(V)): V {.inline.} =
# proc getOrDefault*[K,V](t: T[K,V], key: K, default: V): V {.inline.} =
    mixin withItem
    var defV: V
    let itm: Pair[K,V] = (key, defV)    # WTF default(V) totally breaks
    t.s.withItem(itm, it) do: return it.val
    do: return default

  proc add*[K,V](t: var T[K,V], key: K, val: V) {.inline.} =
    t.s.add((key, val))                 # Add makes this really a multimap

  proc del*[K,V](t: var T[K,V], key: K) {.inline.} =
    discard t.s.missingOrExcl((key, default(V)))

  proc del*[K,V](t: var T[K,V], key: K, had: var bool) {.inline.} =
    had = not t.s.missingOrExcl((key, default(V)))

  proc pop*[K,V](t: var T[K,V], key: K, val: var V): bool {.inline.} =
    var item: Pair[K,V] = (key, default(V))
    result = t.s.take(item)
    val = item.val

  proc take*[K,V](t: var T[K,V], key: K, val: var V): bool {.inline.} =
    var item: Pair[K,V] = (key, default(V))
    result = t.s.take(item)
    val = item.val

  proc pop*[K,V](t: var T[K,V]): Pair[K,V] {.inline.} =
    t.s.pop()

  proc clear*[K,V](t: var T[K,V]) {.inline.} =
    t.s.clear

  proc `$`*[K,V](t: T[K,V]): string =
    if t.len == 0: return "{:}"
    result = "{"
    for key, val in t:
      if result.len > 1: result.add(", ")
      result.addQuoted(key)
      result.add(": ")
      result.addQuoted(val)
    result.add("}")

  #XXX ALGO BUG: As written DOES NOT ACCOUNT FOR DUP KV pairs.  One way to solve
  # this is build two `CountTable[(key,val)]` and compare THEM instead.  Ack.
  #NOTE: Could detect if `<` is available on (key,val) and if so break RH depth
  # ties by that comparison.  That slows point access but makes `data` wholly
  # fixed by membership.  `memcmp(x.data, y.data)` here would then make this
  # slow op 10x faster, but who knows what's rare?  Worth a flag/define guard?
  proc `==`*[K,V](x, y: T[K,V]): bool =
    if isNil(x): return isNil(y)            # 2 nil => true
    if isNil(y): return false               # 1 nil => false
    if x.counter != y.counter: return false # diff size => false
    for key, val in x:                      # diff insert orders => diff `data`
      if not y.hasKey(key) or y.getOrDefault(key) != val: return false
    return true

  proc indexBy*[A, K,V](collection: A, index: proc(x: V): K): T[K,V] =
    result.init
    for item in collection: result[index(item)] = item

  iterator pairs*[K,V](t: T[K,V]): (K,V) =
    for item in t.s: yield (item.key, item.val)

  iterator mpairs*[K,V](t: var T[K,V]): (K, var V) =
    for item in mitems(t.s): yield (item.key, item.val)

  iterator keys*[K,V](t: T[K,V]): K =
    for item in t.s: yield item.key

  iterator values*[K,V](t: T[K,V]): V =
    for item in t.s: yield item.val

  iterator mvalues*[K,V](t: var T[K,V]): var V =
    for item in mitems(t.s): yield item.val

  iterator allValues*[K,V](t: T[K,V]; key: K): V =
    for item in t.s.allItems((key, default(V))): yield item.val

  proc sortByKey*[K,V](t: var T[K,V], order = Ascending) =
    t.s.sort(cmpKey, order)

  proc sortByVal*[K,V](t: var T[K,V], order = Ascending) =
    t.s.sort(cmpVal, order)

  #A few procs to maybe totally obviate CountTable or let it be a type alias
  proc sort*[K,V](t: var T[K,V], order = Descending) {.deprecated:
    "Deprecated since vXX, use 'sortByVal Descending'".} = t.sortByVal(order)

  proc inc*[K,V: SomeInteger](t: var T[K,V], key: K, amount: SomeInteger=1) =
    t.mgetOrPut(key, 0).inc amount

  proc merge*[K,V: SomeInteger](s: var T[K,V], t: T[K,V]) =
    for key, val in t: s.inc(key, val)

  proc valRange*[K,V: SomeInteger](t: T[K,V], above: V=0):
      tuple[min, max: Pair[K,V]] =
    var minKey: K
    var maxKey: K
    var minVal = V.high
    var maxVal = V.low
    for key, val in t:
      if val > above:
        if val <= minVal:
          minKey = key
          minVal = val
        if val >= maxVal:
          maxKey = key
          maxVal = val
 
  proc smallest*[K,V: SomeInteger](t: T[K,V], above: V=0): Pair[K,V] =
    var minKey: K
    var minVal = V.high
    for key, val in t:
      if val > 0 and val <= minVal:
        minKey = key
        minVal = val

  proc largest*[K,V: SomeInteger](t: T[K,V], above: V=0): Pair[K,V] =
    var maxKey: K
    var maxVal = V.low
    for key, val in t:
      if val > 0 and val >= maxVal:
        maxKey = key
        maxVal = val
