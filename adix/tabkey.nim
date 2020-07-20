import hashes

type Pair*[K, V] = tuple[key: K, val: V]

proc hash*[K, V](e: Pair[K, V]): Hash {.inline.} = hash(e.key)
proc `==`*[K, V](a, b: Pair[K, V]): bool {.inline.} = a.key == b.key

proc cmpKey*[K, V](a, b: Pair[K, V]) {.inline.} = cmp(a.key, b.key) # To sort
proc cmpVal*[K, V](a, b: Pair[K, V]) {.inline.} = cmp(a.val, b.val)

proc high*[K, V](e: Pair[K, V]): K {.inline.} = e.key.high
proc low*[K, V](e: Pair[K, V]): K {.inline.} = e.key.low

proc getKey*[K, V](e: Pair[K, V]): K {.inline.} = e.key
proc getKey*(e: SomeInteger): int {.inline.} = int(e)

proc setKey*[K, V](e: var Pair[K, V], f: K) {.inline.} = e.key = K(f)
proc setKey*(e: var SomeInteger, f: SomeInteger) {.inline.} = e = type(e)(f)
