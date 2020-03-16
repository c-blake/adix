# None of these operations need to know about internal representations.  They
# all have the same signatures, semantics & requirements of stdlib.sets.  They
# could mostly be just generic functions (maybe not the operators +/*...) { and
# even work on `seq[T]` if you define the `*OrIncl`s for `seq[T]`. }
import algorithm
template defSetOps*(T: untyped) =

  proc incl*[A](s: var T[A], elt: A) {.inline.} =
    discard s.containsOrIncl(elt)

  proc excl*[A](s: var T[A], elt: A) {.inline.} =
    discard s.missingOrExcl(elt)

  proc incl*[A](s: var T[A], other: T[A]) =
    for elt in other: s.incl elt

  proc excl*[A](s: var T[A], other: T[A]) =
    for elt in other: s.excl elt

  proc pop*[A](s: var T[A], item: var A): bool {.inline.} =
    s.take item

  proc mgetOrIncl*[A](s: var T[A], item: A): var A {.inline.} =
    var dummy: bool
    s.mgetOrIncl(item, dummy)

  proc card*[A](s: T[A]): int = s.len

  proc union*[A](s1, s2: T[A]): T[A] =
    result = s1
    result.incl s2

  proc intersection*[A](s1, s2: T[A]): T[A] =
    result.init(rightSize(min(s1.len, s2.len)))
    for elt in s1:
      if elt in s2: result.incl elt

  proc difference*[A](s1, s2: T[A]): T[A] =
    result.init
    for elt in s1:
      if elt notin s2: result.incl elt

  proc symmetricDifference*[A](s1, s2: T[A]): T[A] =
    result = s1
    for item in s2:
      if result.containsOrIncl(item): result.excl item

  proc `+`*[A](s1, s2: T[A]): T[A] {.inline.} = s1.union s2

  proc `*`*[A](s1, s2: T[A]): T[A] {.inline.} = s1.intersection s2

  proc `-`*[A](s1, s2: T[A]): T[A] {.inline.} = s1.difference s2

  proc `-+-`*[A](s1, s2: T[A]): T[A] {.inline.} = s1.symmetricDifference s2

  proc disjoint*[A](s1, s2: T[A]): bool =
    for item in s1:
      if item in s2: return false
    return true

  proc `<=`*[A](s, t: T[A]): bool =
    if s.counter > t.counter: return false
    for item in s:
      if item notin t: return false
    result = true

  proc `<`*[A](s, t: T[A]): bool =
    s.counter != t.counter and s <= t

  proc `==`*[A](s, t: T[A]): bool =
    s.counter == t.counter and s <= t

  proc map*[A, B](data: T[A], op: proc (x: A): B {.closure.}): T[B] =
    result.init data.len
    for item in data: result.incl op(item)

  proc `$`*[A](s: T[A]): string =
    result = "{"
    for item in s:
      if result.len > 1: result.add(", ")
      result.addQuoted(item)
    result.add("}")

  proc `[]`*[A](s: var T[A], item: A): var A {.inline.} =
    s.withItem(it) do: result = it
    do:
      when compiles($item):
        raise newException(KeyError, "item not found: " & $item)
      else:
        raise newException(KeyError, "item not found")

  proc `to T`*[A](keys: openArray[A]): T[A] =
    result.init rightSize(keys.len)
    for item in keys: result.incl item

  #NOTE: In the stdlib, OrderedSet/OrderedTable provide a way to keep using a
  #table post `sort`.  Here that can be achieved by calling `reIndex` after
  #`sort`.  Since I think it very rare, at least for now we do not auto-call.
  proc sort*[A](s: var T[A], cmp: proc(x,y:A): int, order=SortOrder.Ascending) =
    sort(s.data, cmp, order)  #Includes all empty cells in non-dense variants

  proc hash*[A](s: T[A]): Hash =  #Important to use a COMMUTATIVE combiner
    for i, hc in 0 .. s.hcodes: result = result xor hc
    result = !$result

  # Performance Forensics
  proc depths*[A](s: T[A]): seq[int] =
    ## Compute & return exact distribution of search depths over a set
    for elt in s:
      let d = s.depth(elt)
      if d >= result.len: result.setLen(d + 1)
      result[d] += 1

  proc normalized*[T](x: seq[T]): seq[float] =
    var norm = 0.0
    for n in x: norm += n.float
    norm = 1.0 / norm
    result.setLen x.len
    for i, n in x: result[i] = n.float * norm

  proc depthStats*[A](s: T[A]): tuple[m1, m2: float; mx: int] = # non-central!
    result.m1 = 0.0
    result.m2 = 0.0
    var norm = 0.0
    let ds = s.depths
    for i, d in ds:
      result.m1 += i.float * d.float
      result.m2 += i.float * i.float * d.float
      norm += d.float
    norm = 1.0 / norm
    result.m1 *= norm
    result.m2 *= norm
    result.mx = ds.len
