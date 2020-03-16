import hashes

type Pair*[K, V] = tuple[key: K, val: V]

proc hash*[K, V](e: Pair[K, V]): Hash {.inline.} = hash(e.key)
proc `==`*[K, V](a, b: Pair[K, V]): bool {.inline.} = a.key == b.key

proc cmpKey*[K, V](a, b: Pair[K, V]) = cmp(a.key, b.key)  # For sorting
proc cmpVal*[K, V](a, b: Pair[K, V]) = cmp(a.val, b.val)

proc high*[K, V](e: Pair[K, V]): K {.inline.} = e.key.high
proc low*[K, V](e: Pair[K, V]): K {.inline.} = e.key.low

#These two `getKey` overloads must be in the same module for resolution to work.
proc getKey*[E](e: E): int {.inline.} = e.int
proc getKey*[K, V](e: Pair[K, V]): int {.inline.} = e.key.int

#proc setKey*[E](e: var E, f: SomeInteger) {.inline.} = e = f.int
proc setKey*[K, V](e: var Pair[K, V], f: K) {.inline.} = e.key = K(f)
