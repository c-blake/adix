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

#NOTE: When caller just declares var x: Tab, we cannot control initialization :(
macro doAlias(ns: string, root: string, tabP: string, setP: string) =
  let inline = "{.inline.}"
  parseStmt(&"""
type Tab*[K,V] = {root}{tabP}
type Set*[K]   = {root}{setP}

proc initTab*[K,V](sz=4, numer={ns}Numer, denom={ns}Denom, minFree={ns}MinFree,
                   growPow2={ns}GrowPow2, rehash=rDefault, robinHood=rhDefault):
    Tab[K,V] {inline} =
  init{root}{tabP}(sz, numer, denom, minFree, growPow2, rehash, robinHood)

proc initSet*[K](sz=4, numer={ns}Numer, denom={ns}Denom, minFree={ns}MinFree,
                 growPow2={ns}GrowPow2, rehash=rDefault, robinHood=rhDefault):
    Set[K] {inline} =
  init{root}{setP}(sz, numer, denom, minFree, growPow2, rehash, robinHood)""")

when defined(stdlibTab):
  import tables, sets
  export tables, sets
  type Tab*[K,V] = Table[K,V]
  type Set*[K]   = HashSet[K]
  proc initTab*[K,V](sz=4, numer=1, denom=1, minFree=1, growPow2=1,
                     rehash=false, robinHood=false): Tab[K,V] {.inline.} =
    initTable[K,V](sz)
  proc initSet*[K](sz=4, numer=1, denom=1, minFree=1, growPow2=1, rehash=false,
                   robinHood=false): Set[K] {.inline.} =
    initHashSet[K](sz)
elif defined(directIndex):
  import adix/ditab
  export ditab
  doAlias("di", "DITab", "[K,V]", "[K,void]")
elif defined(orderedLinear):
  import adix/oltabzo # Extra generic params here are void|not sentinel flag, z,
  export oltabzo      #..then number of bits for a hash code in the index part.
  when defined(integerTab0):
    doAlias("ol", "OLTabZO", "[K,V,int,0,8]", "[K,void,int,0,8]")
  elif defined(integerTabM1):
    doAlias("ol", "OLTabZO", "[K,V,int,-1,8]", "[K,void,int,-1,8]")
  else:
    doAlias("ol", "OLTabZO", "[K,V,void,0,8]", "[K,void,void,0,8]")
else:
  import adix/lptabz  # Extra generic params here are void|not sentinel flag, z.
  export lptabz
  when defined(integerTab0):
    doAlias("lp", "LPTabZ", "[K,V,int,0]", "[K,void,int,0]")
  elif defined(integerTabM1):
    doAlias("lp", "LPTabZ", "[K,V,int,-1]", "[K,void,int,-1]")
  else:
    doAlias("lp", "LPTabZ", "[K,V,void,0]", "[K,void,void,0]")
