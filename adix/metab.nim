## This module provides an easy way to do compile-time switched impl swaps for
## various table/set reprs with various compile-time switched defaults.
import macros, strformat

when defined(noRehash):
  let rDefault = false
else:
  let rDefault = true

when defined(noRobinHood):
  let rhDefault = false
else:
  let rhDefault = true

proc rightSz*(x: Natural): int {.inline,deprecated: "Only identity now".} = x

macro doAlias(ns: string, root: string, tabP: string, setP: string) =
  let inline = "{.inline.}"
  parseStmt(&"""
type Tab*[K,V] = {root}{tabP}
type Set*[K]   = {root}{setP}

proc initTab*[K,V](sz={ns}InitialSize, numer={ns}Numer, denom={ns}Denom,
                   minFree={ns}MinFree, growPow2={ns}GrowPow2, rehash=rDefault,
                   robinHood=rhDefault): Tab[K,V] {inline} =
  init{root}{tabP}(sz, numer, denom, minFree, growPow2, rehash, robinHood)

proc initSet*[K](sz={ns}InitialSize, numer={ns}Numer, denom={ns}Denom,
                 minFree={ns}MinFree, growPow2={ns}GrowPow2, rehash=rDefault,
                 robinHood=rhDefault): Set[K] {inline} =
  init{root}{setP}(sz, numer, denom, minFree, growPow2, rehash, robinHood)""")

when defined(axStdlib):  #NOTE: stdlib version cannot ctrl, e.g. `initialSize`
  import tables, sets     #      when client just declares `var x: Tab`.
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
  type Null = distinct int8 # 28 bits blocks hash() calls up to 1/4 GiEntries
  doAlias("lp", "LPTabz", "[K,V,Null,28]", "[K,void,Null,28]")
else:
  import adix/lptabz  # Extra generic params here are void|not sentinel flag, z.
  export lptabz
  when defined(axIntTab0):
    doAlias("lp", "LPTabz", "[K,V,K,0]", "[K,void,K,0]")
  elif defined(axIntTabM1):
    doAlias("lp", "LPTabz", "[K,V,K,-1]", "[K,void,K,-1]")
  else:
    doAlias("lp", "LPTabz", "[K,V,void,0]", "[K,void,void,0]")
