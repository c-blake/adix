## This module provides an easy way to do compile-time switched impl swaps for
## various table/set reprs with various compile-time switched defaults.  You
## should really just learn how to use `LPTabz[..]` directly, though.
import core/macros, std/strformat

when defined(axRehash):
  let rDefault = true
else:
  let rDefault = false

when defined(axRobinHood):
  let rhDefault = true
else:
  let rhDefault = false

proc rightSz*(x: Natural): int {.inline,deprecated: "Only identity now".} = x

macro doAlias(ns: string, root: string, tabP: string, setP: string) =
  let inline = "{.inline.}"
  parseStmt(&"""
type Tab*[K,V] = {root}{tabP}

proc initTab*[K,V](sz={ns}InitialSize, numer={ns}Numer, denom={ns}Denom,
                   minFree={ns}MinFree, growPow2={ns}GrowPow2, rehash=rDefault,
                   robinHood=rhDefault): Tab[K,V] {inline} =
  result.init(sz, numer, denom, minFree, growPow2, rehash, robinHood)

proc toTab*[K,V](pairs: openArray[(K,V)], dups=false): Tab[K,V] =
  result.init pairs.len     # calling to{root}{tabp}(pairs, dups) fails; mixin?
  if dups:
    for k, v in items(pairs): result.add(k, v)
  else:
    for k, v in items(pairs): result[k] = v

type Set*[K] = {root}{setP}

proc initSet*[K](sz={ns}InitialSize, numer={ns}Numer, denom={ns}Denom,
                 minFree={ns}MinFree, growPow2={ns}GrowPow2, rehash=rDefault,
                 robinHood=rhDefault): Set[K] {inline} =
  result.init(sz, numer, denom, minFree, growPow2, rehash, robinHood)

proc toSet*[K](keys: openArray[K], dups=false): Set[K] =
  result.init keys.len      # calling to{root}{tabp}(pairs, dups) fails; mixin?
  if dups:
    for k in keys: result.add k
  else:
    for k in keys: result.incl k""")

when defined(axStdlib):     #NOTE: stdlib version cannot ctrl, eg. `initialSize`
  import std/[tables, sets] #      when client just declares `var x: Tab`.
  export tables, sets
  type Tab*[K,V] = Table[K,V]
  type Set*[K]   = HashSet[K]
  proc initTab*[K,V](sz=4, numer=1, denom=1, minFree=1, growPow2=1,
                     rehash=false, robinHood=false): Tab[K,V] {.inline.} =
    initTable[K,V](sz)
  proc initSet*[K](sz=4, numer=1, denom=1, minFree=1, growPow2=1, rehash=false,
                   robinHood=false): Set[K] {.inline.} =
    initHashSet[K](sz)
elif defined(axDirect):
  import adix/ditab
  export ditab
  doAlias("di", "DITab", "[K,V]", "[K,void]")
elif defined(axInOrder):
  import adix/lptabz  # Extra generic params are void|not|order sentinel flag,
  export lptabz       #..then z|num bits for a hash code in the index part.
  type InsOrd = distinct int8 # 8 bits blocks most dbl indirections on misses
  doAlias("lp", "LPTabz", "[K,V,InsOrd,8]", "[K,void,InsOrd,8]")
else:
  import adix/lptabz  # Extra generic params here are void|not sentinel flag, z.
  export lptabz
  when defined(axIntTab0):
    doAlias("lp", "LPTabz", "[K,V,K,0]", "[K,void,K,0]")
  elif defined(axIntTabM1):
    doAlias("lp", "LPTabz", "[K,V,K,-1]", "[K,void,K,-1]")
  else:
    doAlias("lp", "LPTabz", "[K,V,void,0]", "[K,void,void,0]")
