## I know of no other as-simple/general FOSS B-Tree (in any prog.langs|books).
## Theory people recurse; DB code clutters w/sync; Other APIs are less general.
## Correct me if I am wrong/cite this if it be your starting point.  Little
## effort is made to explain these algos in comments as it's impractical to cram
## a course into this file with no figures.  More details & resources are at
## https://en.wikipedia.org/wiki/B-tree or in Graefe & Kuno 2011.
##
## This module defines a template that defines procs on a pretty general B-Tree.
## The tree can be positional-only, keyed-only or keyed-ranked, be either set of
## keyed rows or (key,value)-style, have its nodes allocated in any way (via
## abstract ptrs & deref, eg. on disk via `memfiles`, via node pools hanging
## off another object, or GC'd refs), & manage dup keys either inlined into the
## structure or handled externally (within the same rank space!).  This is >36
## (3rk*2kv*3alloc*2du) styles from one instantiation harness.  The template has
## many parameters to control all these choices.  All but `Ob` are defaulted:
##
## - `K`: Key type; Caller provides `getKey(Ob):K`; void if positional-only
## - `Pos`: type for node weight/position augmentation; void if keyed-only
## - `ObSize`: type for dup chain size if handled externally; void if embedded
## - `nodeSize`: size in bytes of a node.  B-Tree Order `m` is inferred from
##   this.  Our notation is m..2m links split by m-1..2m-1 obs; 2 => a 234-tree;
##   2*m**(h-1)-1 <= numNodes <= (2m)**h-1 where h=height of tree.
## - `Ix`: type for object/link indices into node arrays
## - `Ln`: type for the link array in nodes
## Common notation/abbreviations in this code/APiS:
## - `s|side`: 0|1 `bool` side of an op. Eg. successor = path.seekAdj(true).
## - `Ob|ob`: abstract object type being stored or an instance/array of it.
## - `Ln|ln`: abstract link type being stored or an instance/array of it.
## - `allocProcs` (btPool|btRef|btFile): allocation style for outer Nodes
## - `nodeProcs` (btLinearNode|btNestedNode): in-Node organization style
##
## Notable implementation choices:
## ======================================================
## There is no "Tree" type distinct from the "SubTree/Node" type.  Once made a
## root node never moves.  That root address is the only handle needed.  "nil"
## ptrs (in whatever allocation arena is used) are just empty trees.  Because
## linear ordering always has exactly 2 sides, parameterization into `s|side`
## often keeps life simple/organized (cf. Mehlhorn DS&Algos|common sense).
##
## Routines are all non-recursive.  Instead a `Path` object is central to the
## API & we clearly separate cursor manipulation from mutation.  This also makes
## the 3 main styles (ranked-only, keyed-only, keyed-ranked) symmetric & removes
## recursion overhead (big for tall trees/small `m`).  Each instance can be a
## multiset/table with "sided" edits (stack/queue) of duplicate key series.
##
## Victim replacement selection in internal deletes biases toward uniform node
## occupancy rather than minimum node count.  The bulk loader to build a minimum
## height tree from pre-ordered inputs also allows leaving 1 (generalizable to
## x?) spare slot in each node to speed early inserts later.  A property check
## routine is provided for would-be extenders.  There is presently no provision
## for for concurrent access as the focus is just a good single-threaded tree.
##
## Limitations/future work:
## ======================================================
## One limitation is that leaf&internal nodes have the same size&representation,
## wasting `~4..8*m` B/leaf.  This is <33% waste for `Ob` >=8..16 bytes.  (Post
## aging, occupancy =~69% anyway.)  This cost is spent to avoid complexity of
## either two node allocation pools with dynamic conversion or different-`m`
## orders for leaf & internal nodes.  *Max* data density means wider 4..7 node
## split-merges (not 2..3) & specializing on key types, anyway; Eg. for string
## keys not duplicating prefixes in a leaf between ("aab1", "aab2").
##
## This is a work in progress. Algorithmically, we should maybe do A) whole tree
## `catenate` (helpful for seq style `&`), B) more complex whole tree `split` &
## C) optimizations for giant dup blocks.  Nim TODOs incl: make easy to include
## inside other generic types, add easy HashSet & Table uses in terms of this
## lower-level core, make run-tm errs compile-tm, do GC'd&memfiles Ln variants,
## do distinct int `Ln` for overload/figure out exports & BIG1 - `btNestedNode`
## alloc option w/Path[].i -> subpath elsewhere for good avg case edit scaling
## {log_2(m)*log_m(N)=log_2(N)}.  Even DRAM has ideal node size ~ `70ns*40GB/s =
## 2800B/(2+4)=~466`, though `moveMem` can have great constant factors.
when not declared(stderr): import std/syncio

proc orderFit*(target, wtSz, ixSz, obSz, lnSz: int): int =
  result = 2        # *Minimum* branching supported is a 2-3-4 tree
  while wtSz + 2*ixSz + (2*(result+1) - 1)*obSz + 2*(result+1)*lnSz < target:
    result.inc      # Above calc assumes a packed node; Also, `void.sizeof==0`.

template btPool*(Ob, Pos: type; nodeSize: int; Ix, Ln: type) =
  ## A simple pool allocator.  Easily adapted to `memfiles` | GC'd allocators.
  # TODO: which we should also provide in this module for ease of use.
  const m = Ix(orderFit(nodeSize, Pos.sizeof, Ix.sizeof, Ob.sizeof, Ln.sizeof))
  proc btOrder(): Ix = m
  type
    Node {.inject.} = object
      when Pos isnot void:
        wt: Pos                         # Rank augment is cheap, but optional
      nO, nL: Ix                        # Could have just one but for bulk load
      ob: array[2*m - 1, Ob]
      ln: array[2*m    , Ln]            # Leaf nodes need no ln[]; See CHOICES
    Path {.inject.} =                   # Basic handle/itr type; ob=[^1].ob[i]
      seq[tuple[p: Ln, i: Ix]]

  let Ln0 {.inject.} = Ln(0)            # Reserve [0] node; Could make Ln.high

  var pool = newSeq[Node](65536)        #XXX size dynamically; some kinda handle
  var nodeH = Ln(1)

  for i in Ln(1) ..< Ln(pool.len - 1):  # Thread the free list
    pool[i].ln[0] = i + 1
  pool[^1].ln[0] = Ln0

  proc `[]`(p: Ln): var Node = pool[int(p)] # `[]` derefs `Ln` to a `Node`

  proc copy(p, q: Ln) = pool[int(p)] = pool[int(q)]

  proc newN(): Ln =                     # alloc a Node
    result = nodeH
    result[].nL = 0; result[].nO = 0
    when Pos isnot void:
      result[].wt = 0
    nodeH = pool[nodeH].ln[0]

  proc freeN(p: Ln) =                   # release a Node
    p[].ln[0] = nodeH
    nodeH = p

  when defined(btRefCk):
    proc mark(t: Ln, inTr: var seq[bool]) =
      if t == Ln0: return
      inTr[t] = true                                    # Mark self..
      for i in 0.Ix ..< t[].nL: mark(t[].ln[i], inTr)   #..& kids

    proc refCk(t: Ln): bool {.used.} =
      var inTree = newSeq[bool](pool.len) # Mark&sweep style mem check
      t.mark inTree
      var isFree = newSeq[bool](pool.len)
      var p = nodeH                       # Chase free list to mark free
      while p != Ln0:
        isFree[p] = true
        p = p[].ln[0]
      for p in 1 ..< pool.len - 1:        # Cross-check two accountings
        if inTree[p] and isFree[p]:
          stderr.write "node " & $p & " in tree but also free\n"; return true
        if not inTree[p] and not isFree[p]:
          stderr.write "node " & $p & " not in tree or free\n"; return true
  else:
    proc refCk(t: Ln): bool {.used.} = false

template btLinearNode*(Ob, K, Pos: type; M: untyped; Ix, Ln: type) = ## Node ops
  const m = Ix(M)
  proc oPut(t: Ln, i: Ix, ob: Ob) =
    if i < t[].nO:
      moveMem t[].ob[i + 1].addr, t[].ob[i].addr, int(t[].nO - i)*Ob.sizeof
    t[].nO.inc
    t[].ob[i] = ob
  
  proc lPut(t: Ln, i: Ix, ln: Ln) =
    if i < t[].nL:
      moveMem t[].ln[i + 1].addr, t[].ln[i].addr, int(t[].nL - i)*Ln.sizeof
    t[].nL.inc
    t[].ln[i] = ln
  
  proc oDel(t: Ln, i: Ix) =
    if t[].nO - i > 1:
      moveMem t[].ob[i].addr, t[].ob[i + 1].addr, int(t[].nO - i - 1)*Ob.sizeof
    t[].nO.dec
  
  proc lDel(t: Ln, i: Ix) =
    if t[].nL - i > 1:
      moveMem t[].ln[i].addr, t[].ln[i + 1].addr, int(t[].nL - i - 1)*Ln.sizeof
    t[].nL.dec
  
  # split kid t.ln[i]; Lift kid.ob[m-1] to t.ob[i],Make new 1-sib w/rest obs&lns
  # NOTE: 2*m-1 is odd in full node => median always perfectly cleaves kid&sib.
  proc split(t: Ln, i: Ix) =
    let kid = t[].ln[i]
    let sib = newN()
    oPut(t, i, kid[].ob[m - 1])         # Split ob->parent
    lPut(t, i + 1, sib)                 # Link in sib
    sib[].nO = kid[].nO - m             # Adj for median+kid
    copyMem sib[].ob[0].addr, kid[].ob[m].addr, int(sib[].nO)*Ob.sizeof # Cp 1-s
    kid[].nO = m - 1                    # Clip kid obs
    if not kid.isLeaf:                  # Cp ln[], too
      sib[].nL = kid[].nL - m
      copyMem sib[].ln[0].addr, kid[].ln[m].addr, int(sib[].nL)*Ln.sizeof
      kid[].nL = kid[].nO + 1
  
  # Drop t.ob[i] to t.ln[i], then cp all t.ln[i+1]s data & release t.ln[i+1]
  proc merge(t: Ln, i: Ix) =
    let kid = t[].ln[i]; let sib = t[].ln[i + 1]
    copyMem kid[].ob[kid[].nO  ].addr, t[].ob[i].addr  , Ob.sizeof
    copyMem kid[].ob[kid[].nO+1].addr, sib[].ob[0].addr, int(sib[].nO)*Ob.sizeof
    copyMem kid[].ln[kid[].nO+1].addr, sib[].ln[0].addr, int(sib[].nL)*Ln.sizeof
    kid[].nO = kid[].nO + sib[].nO + 1
    kid[].nL = kid[].nL + sib[].nL
    oDel(t, i)
    lDel(t, i + 1)
    freeN(sib)
  
  when K isnot void:
    const binSearch = 32           # nO > this use binary-search not linear-scan
    let binSearch1 = Ix(binSearch div 2)
    proc ix(t: Ln, side: bool, searchKey: K): Ix =
      var hi = t[].nO
      if hi < binSearch1 and side: # side => 1+biggest ix where LT|hi if all LT
        for lo in countdown(hi, 1):
          if t[].ob[lo - 1].getKey < searchKey: return lo
        return 0
      elif hi < binSearch and not side:
        for lo in 0.Ix ..< hi:     # !side => least ix where !LT|hi if all LT
          if t[].ob[lo].getKey >= searchKey: return lo
        return hi
      else:                        # binsearch can maybe be optimized for edges
        var lo = 0.Ix              # Eg. maybe spend 1 [0] vs [nO-1] compares?
        while lo < hi:
          let mid = (lo + hi) div 2
          if t[].ob[mid].getKey < searchKey: lo = mid + 1
          else: hi = mid
        return lo

template defBTree*(Ob: type, K: type=void, Pos: type=void, ObSize: type=void,
                   nodeSize: int=16, Ix: type=int16, Ln: type=uint16,
                   allocProcs=btPool, nodeProcs=btLinearNode) =
  when K isnot void:              # proc getKey(Ob) must get a K from an Ob
    when type(cast[ptr Ob](1'u)[].getKey) isnot K:  # ensure user defs accessor
#XXX Should be compile-time but import std/macros for error breaks, eg. `copy`.
      stderr.write "must define `getKey(Ob)`\n"
  when ObSize isnot void:         # proc size(Ob) must get an ObSize from an Ob
    when type(cast[ptr Ob](nil)[].size) isnot ObSize:
      stderr.write "must define `size(Ob)`\n"
    when type(cast[ptr Ob](nil)[].size) isnot Ordinal:
      stderr.write "`size(Ob)` must return an integer\n"

  allocProcs(Ob, Pos, nodeSize, Ix, Ln) # injects: Node, Path, Ln0, btOrder()
  const m = btOrder()
  proc isLeaf(p: Ln): bool {.inline.} = p[].nL == 0
  proc isFull(p: Ln): bool {.inline.} = p[].nO == 2*m - 1
  nodeProcs(Ob, K, Pos, m, Ix, Ln)

  proc seekMost(t: Ln, path: var Path, side: bool): bool {.discardable.} =
    # Extend `path` to `side`-most (false=min; true=max) edge of subtree @`t`
    if t == Ln0:                        # Null Tree
      return false
    if t[].nO == 0:                     # Empty Root => Empty Tree
      path = @[(t, 0.Ix)]
      return false
    var p = t
    while not p.isLeaf:
      path.add (p, if side: p[].nL - 1 else: 0)
      p = p[].ln[path[^1].i]
    path.add (p, if side: p[].nO - 1 else: 0)
    return true
  
  proc seekAdj(path: var Path, side: bool): bool {.discardable.} =
    # Set `path` to `side`-next (false = <; true= !<) element
    if path.len == 0:                   # In case user calls after ==0 EOTree
      return false                      # They should not, but eh.
    if not path[^1].p.isLeaf:
      path[^1].i += Ix(side)
      return seekMost(path[^1].p[].ln[path[^1].i], path, not side)
    while path[^1].i == (if side: path[^1].p[].nO - 1 else: 0):
      if path.len == 0: return false    # path[] @end; No more cessors
      discard path.pop
      if path.len == 0: return false    # path[] @end; No more cessors
      path[^1].i -= Ix(side)  # Don't skip visit of internal node after its kids
    path[^1].i += 2*Ix(side) - 1
    return true

  iterator nodes(t: Ln): tuple[p: Ln, ob: Ob, depth: int] = # structural iter
    var path: Path
    t.seekMost(path, false)             # start at 0-edge; advance to 1-adjacent
    while path.len > 0:
      yield (path[^1].p, path[^1].p[].ob[path[^1].i], path.len - 1)
      path.seekAdj true
  
  iterator items*(t: Ln): Ob =                              # object iter
    var path: Path
    t.seekMost(path, false)             # start at 0-edge; advance to 1-adjacent
    while path.len > 0:
      yield path[^1].p[].ob[path[^1].i]
      path.seekAdj true
  
  when K isnot void:
    proc seekKey(t: Ln, path: var Path, searchKey: K): bool{.used,discardable.}=
      if t == Ln0: return false         # Empty tree
      path.setLen 0
      var p = t
      while true:
        let i = ix(p, false, searchKey)
        path.add (p, i)                 # Record visit;ob ix to ins@,if desired
        if i < p[].nO and p[].ob[i].getKey == searchKey:
          return true                   # Found!
        if p.isLeaf:                    # Leaf, but not found above; i=ins spot
          return false
        p = p[].ln[i]                   # Descend
  
    # Well-defined sub-orders help when dup keys can occur.  seekKeys sets path
    # to side==false|true-most key.  Eg., (seekKeys(1)+add, seekKeys(0)+del)
    # yields FIFO while (seekKeys(0)+add, seekKeys(0)+del) yields LIFO.
    proc seekKeys(t: Ln, path: var Path, side: bool, searchKey: K):bool{.used.}=
      if t == Ln0: return false         # Empty tree
      path.setLen 0
      var p = t
      while true:
        let i = ix(p, side, searchKey)
        path.add (p, i)                 # Record visit;ob ix to ins@,if desired
        if i < p[].nO and p[].ob[i].getKey == searchKey:
          if side or not p.isLeaf:      # Found: scan side-cessors to EOT|diff K
            while seekAdj(path, side):  #TODO Hop giant dup blks?
              if path[^1].p[].ob[path[^1].i].getKey != searchKey:
                break
            if path.len > 0:            # Diff key; reverse scan by 1
              seekAdj(path, not side)
            else:                       # End of tree; re-set path to side-most
              seekMost(t, path, side)
          return path.len > 0
        if p.isLeaf:                    # Leaf, but not found above
          return false
        p = p[].ln[i]                   # Descend
  
  when Pos isnot void:
    proc seekNth(t: Ln, path: var Path, n: int): int {.used.} =
      # Returns rank of path byproduct when ObSize isnot void.  Else, just 0|-1
      if t == Ln0: return 0                 # Null tree
      var n = if n < 0: t[].wt + n else: n  # Py-like negative n
      if t[].nO == 0 or n < 0 or n >= t[].wt:
        path = @[(t, 0.Ix)]                 # Various out of bounds
        return -1
      path.setLen 0
      when ObSize isnot void:
        result = n + 1
      var p: Ln
      var t = t
      while true:
        if t.isLeaf:
          when ObSize isnot void:
            var i = 0
            while true:
              when defined(btBug):            # WTF: 1st branch works elsewhere
                n -= int(t[].ob[i].size)      #   ..in this VERY SAME proc, and
              else:                           #   ..also on eg., nim-0.20.2
                n -= int(t[].ob[i..i][0].size)# Work around crazy bug
              if n < 0: break
              i.inc
            path.add (t, i.Ix)
            result -= n + 1
          else:
            path.add (t, n.Ix)
          return
        var i = 0.Ix
        while i < t[].nO:                   # Non-leaf; find ln to chase, `p`
          p = t[].ln[i]
          if p[].wt > n:
            break
          n -= p[].wt                       # Count i-th subtree
          if n <= 0:                        # Got the item we wanted
            path.add (t, i)
            when ObSize isnot void:
              result += int(t[].ob[i].size) - 1
            return
          when ObSize isnot void:
            n -= int(t[].ob[i].size)
            if n < 0:                       # Reverse i-th key in node
              path.add (t, i)
              return result - (n + 1)
          else:
            n.dec
          i.inc
        if i >= t[].nO:                     # Descend into 1-most subtree
          p = t[].ln[i]
        path.add (t, i)
        t = p
  
    proc rank(path: var Path): int =
      ## 1-origin rank. Inverse of seekNth. Returns weight<=elt path points to.
      if path.len == 0 or path[0].p == Ln0 or path[0].p[].nO == 0: # Empty tree
        return                          # Only tm zero returned (for valid path)
      result = path[0].p[].wt                       # Start with total num(obs)
      when ObSize isnot void:
        for e in path:
          if not e.p.isLeaf:                            # Subtract >= kids
            for j in e.i + 1 ..< e.p[].nL:
              result -= e.p[].ln[j][].wt
          for k in countdown(e.p[].nO - 1, e.i):
            result -= int(e.p[].ob[k].size)
        result += int(path[^1].p[].ob[path[^1].i].size) # Add back end of path
      else:
        for e in path:
          if not e.p.isLeaf:                            # Subtract >= kids
            for j in e.i + 1 ..< e.p[].nL:
              result -= e.p[].ln[j][].wt
          result -= int(e.p[].nO - e.i)                 # Subtract >= obs
        result += 1                                     # Add back end of path
  
  # 3 helper procs for `proc add`
  proc seekPush(path: var Path, side: bool): bool {.discardable, inline.} =
    if path.len == 0 or path[0].p == Ln0 or path[0].p[].nO == 0:
      return false
    if path[^1].p.isLeaf:
      path[^1].i += Ix(side)
      return true
    seekAdj(path, side)                 # Move to side-cessor, always in a leaf
    path[^1].i += Ix(not side)
    return true
  
  proc splitAux(t: Ln, i: Ix, sz: int) =
    discard                             # Post split metadata updates
    when Pos isnot void:
      let kid = t[].ln[i]
      let sib = t[].ln[i + 1]
      when ObSize isnot void:           # Obs in sib itself
        var w = Pos(0)
        for j in 0.Ix ..< sib[].nO: w += Pos(sib[].ob[j].size)
      else:
        var w = Pos(sib[].nO)           # Obs in sib itself
      for j in 0.Ix ..< sib[].nL:       # Aggregate w over subtree(sib)
        w += sib[].ln[j][].wt
      sib[].wt  = w                     # Set sib
      kid[].wt -= w + Pos(sz)           # Adjust kid
  
  proc add(path: var Path, ob: Ob, side=true, keyPresent=false) {.used.} =
    if keyPresent: path.seekPush(side)
    var d = path.len - 1                # [d, path.len) brackets nodes to split
    while d > 0 and path[d].p.isFull:
      d.dec
    if d == 0 and path[0].p.isFull:     # ROOT SPLIT
      let kid = newN()
      let t   = path[0].p
      copy kid, t                       # Node copy here enables FIXED ROOT ADDR
      when ObSize isnot void:
        let sz = int(t[].ob[m - 1].size)
      else:
        let sz = 1
      t[].nO    = 0                     # Empty Root Node, pointing to new kid
      t[].nL    = 1
      t[].ln[0] = kid
      when Pos isnot void:
        t[].wt = kid[].wt               # Fix-up metadata: weights & up links
      split(t, 0)                       # Split old root node, pulling median up
      splitAux(t, 0, sz)
      path.setLen path.len+1
      moveMem path[1].addr, path[0].addr, path.len*path[0].sizeof
      d.inc
      path[0].p = t                     # This should always be true already.
      if path[1].i < m:                 # Repair path[]
        path[0].i = 0
        path[1].p = t[].ln[0]
      else:
        path[0].i = 1
        path[1].p = t[].ln[1]
        path[1].i -= m
    while d < path.len - 1:
      when ObSize isnot void:
        let sz = int(path[d].p[].ln[path[d].i][].ob[m - 1].size)
      else:
        let sz = 1
      split(path[d].p, path[d].i)
      splitAux(path[d].p, path[d].i, sz)
      if path[d+1].i >= m:              # Repair path[]
        path[d+1].p = path[d].p[].ln[path[d].i + 1]
        path[d+1].i -= m
        path[d].i.inc
      d.inc
    when Pos isnot void:                # Update weights
      for k in 0 ..< path.len: path[k].p[].wt += 1
    oPut(path[^1].p, path[^1].i, ob)
  
  proc del(path: var Path) {.used.} =
    let r = path[0].p                   # Root of tree
    var t = r
    if not path[^1].p.isLeaf:           # Internal: Lift BIGGER neighbor
      var nbor: array[2, Path]          # Intrn node ob ALWAYS has BOTH 01 nbors
      nbor[0] = path; nbor[0].seekAdj(false)         # Predecessor Calc
      nbor[1] = path; nbor[1].seekAdj(true)          # Successor Calc
      let s = nbor[1][^1].p[].nO > nbor[0][^1].p[].nO # s = BIGGER node
      let si = int(s)
      let np = nbor[si][^1].p           # copy bigger-scessor to victim spot
      let ni = nbor[si][^1].i
      path[^1].p[].ob[path[^1].i] = np[].ob[ni]
      when ObSize isnot void:           # Fix wt along new path
        for j in path.len ..< nbor[si].len:
          nbor[si][j].p[].wt -= Pos(np[].ob[ni].size - 1) #[]#
      path = nbor[si]                   # Change path to nbor..
    oDel(path[^1].p, path[^1].i)        # Delete ob now guaranteed to be in leaf
    when Pos isnot void:
      for e in path: e.p[].wt -= 1      # Common case here & fix-up rare cases
    var sib: array[2, Ln]               # which sib
    var xib, zib: array[2, Ix]          # which ix, which node size
    for d in countdown(path.len - 1,1): # Bottom up, below root
      t = path[d].p
      if t[].nO >= m - 1: break         # No under-flow => done!
      let p = path[d-1].p
      let i = path[d-1].i
      if i > 0:                         # Arrange to steal from BIGGEST sib
        xib[0] = i - 1
        sib[0] = p[].ln[xib[0]]
        zib[0] = sib[0][].nO
      else:
        zib[0] = 0                      # Ensure sib !selected for rot|merge
        xib[0] = 0
      if i < p[].nO:
        xib[1] = i + 1
        sib[1] = p[].ln[xib[1]]
        zib[1] = sib[1][].nO
      else:
        zib[1] = 0                      # Ensure sib !selected for rot|merge
        xib[1] = p[].nL - 1
      let s = zib[1] > zib[0]           # Set side of biggr;[szx]ib=ptrs,szs,ixs
      let si = int(s)
      if zib[si] >= m:                  # Biggest-sib s has extra => Take 1
        let sMost   = if s: t[].nO else: 0.Ix
        let sMostL  = if s: t[].nL else: 0.Ix
        let sLeast  = if s: 0.Ix else: sib[si][].nO - 1
        let sLeastL = if s: 0.Ix else: sib[si][].nL - 1
        let ip      = i - Ix(not s)
        oPut(t, sMost, p[].ob[ip])      # Parent
        if not sib[si].isLeaf:
          lPut(t, sMostL, sib[si][].ln[sLeastL])  # Sib kid
        p[].ob[ip] = sib[si][].ob[sLeast]         # sep<-sLeast
        when Pos isnot void:
          let subtree = if t.isLeaf: Pos(0) else: t[].ln[sMostL][].wt
        when ObSize isnot void:
          t[].wt       += subtree + Pos(t[].ob[sMost].size)
          sib[si][].wt -= subtree + Pos(sib[si][].ob[sLeast].size)
        elif Pos isnot void:
          t[].wt       += subtree + 1
          sib[si][].wt -= subtree + 1
        oDel(sib[si], sLeast)           # del sLeast
        if not sib[si].isLeaf:
          lDel(sib[si], sLeastL)        # del sLeast
        break
      let ix = xib[0]       # Now both zib[0]s are minimal; Merge t, sep, alt
      when ObSize isnot void:
        p[].ln[ix][].wt += Pos(p[].ob[ix].size) + p[].ln[ix + 1][].wt
      elif Pos isnot void:
        p[].ln[ix][].wt += 1 + p[].ln[ix + 1][].wt
      merge(p, ix)                      # Also frees .ln(p, ix + 1)
      path[d].p = p[].ln[ix]
      if path[d-1].i > 0:
        path[d-1].i.dec
    if r[].nO == 0 and not r.isLeaf:    # Root empty but NOT a leaf
      let old = r[].ln[0]               # Save old ptr to free
      copy r, r[].ln[0]                 # Copy 0-most kid to T
      freeN(old)                        #..then release space
  
  # When all adds are @s-edge, instead of even splits, leave !s-side FULL & make
  # empty s-node.  Caller inits path to @[(root,0)] at start.  When done, tree
  # is mostly full, but badd1Done must be called to establish tree invariants.
  proc badd1(path: var Path, ob: Ob, t: Ln = 0, spare=0) =
    let spare = min(spare, 1) #m - 2?) #XXX   # Need >= m-1 obs/m lns per node
    if path.len == 0:                   # Very first iteration
      path = @[ (t, 0.Ix) ]             # Only tm need root `t` vs just path[0]
    elif (not path[^1].p.isLeaf) and path[^1].p[].nL < 2*m:
      var p = path[^1].p
      while not p.isLeaf:
        let nwN = newN()                # Go down tree, making new nodes
        lPut(path[^1].p, path[^1].i + 1, nwN)
        path.add (nwN, 0.Ix)
        p = p[].ln[0]
    elif path[^1].p[].nO != Ix(2*m - 1) - Ix(spare):
      path[^1].i.inc
    else:
      let t = path[0].p
      var last: Ln
      while path.len > 0 and path[^1].p[].nO == Ix(2*m - 1) - Ix(spare):
        let eop = path.pop
        last = eop.p
      if path.len == 0:                 # Root split
        let kid = newN()
        copy kid, t                     # Rare cp here enables FIXED ROOT ADDR
        t[].nO  = 0                     # oPut will update t[].nO
        t[].nL  = 1
        t[].ln[0] = kid
        when Pos isnot void:
          t[].wt = kid[].wt             # Fix-up metadata: weights & up links
        path.add (t, 0.Ix) # Bulk mode nw root always empty&0 elt always succssr
      else:                             # non-Root: init to zero or incr index
        path[^1].i = if last != path[^1].p[].ln[0]: path[^1].i + 1 else: 0
    when Pos isnot void:                # Update weights
      for k in 0 ..< path.len: path[k].p[].wt += 1
    oPut(path[^1].p, path[^1].i, ob)
  
  # Re-distribute s-most sep and its 2 kids to restore fullness of s-side.  This
  # generalizes rotation from rich sib in `del` to move up to m-1 elts (vs 1).
  proc restore1(t: Ln) =
    let i   = t[].nO - 1                # top down ensures t.nO > 0
    let kid = t[].ln[i]
    let sib = t[].ln[i + 1]
    if m - 1 <= sib[].nO: return
    let nNeed = m - 1 - sib[].nO
    when Pos isnot void:
      var dw = Pos(nNeed)
    oPut(sib, 0, t[].ob[i])
    for j in 0.Ix ..< nNeed - 1:
      oPut(sib, 0, kid[].ob[kid[].nO - 1 - j])
    if not kid.isLeaf:
      for j in 0.Ix ..< nNeed:
        lPut(sib, 0, kid[].ln[kid[].nL - 1 - j])
        when Pos isnot void:
          dw += sib[].ln[0][].wt
      kid[].nL -= nNeed
    t[].ob[i] = kid[].ob[kid[].nO - nNeed]
    kid[].nO -= nNeed
    when Pos isnot void:
      kid[].wt -= dw
      sib[].wt += dw
  
  # Only s-most nodes on each level may be <FULL-spare, but can even be EMPTY.
  proc badd1Done(t: Ln, spare=0) =
    if t == Ln0 or t.isLeaf: return     # Only a 1-level tree/root leaf
    var path: Path
    t.seekMost(path, false)
    var p = t
    let h = path.len
    var d = 0
    while p[].nL == p[].nO + 1:
      path[d].p = p
      path[d].i = p[].nL - 1
      p = p[].ln[path[d].i]
      d.inc
    path[d] = (p, p[].nO - 1)
    d.inc
    for i in 0 ..< h - 1:               # top-dn redistrib s-most sep&its 2 kids
      if i >= d - 1:                    # Create s-most kids all the way down.
        lPut(path[i].p, path[i].p[].nL, newN())
      restore1(path[i].p)
      path[i + 1].p = path[i].p[].ln[path[i].p[].nL - 1]
  
  # Complement `badd0` not yet written satisfactorily.  User can always iterate
  # backwards over inputs via countdown/etc. (at some expense, obviously).
  proc badd(path: var Path, ob: Ob, s=true, t: Ln = 0, spare=0) {.used.} =
    if s: badd1(path, ob, t, spare)
  proc baddDone(t: Ln, s=true, spare=0) {.used.} =
    if s: badd1Done(t, spare)

  proc check(t: Ln): int {.used.} =               # CHECK TREE INVARIANTS
    var height = -1 
    var nErr = 0

    proc err(t: Ln, depth: int, msg: string) =
      stderr.write "\e[37;44m d:", depth, " p:", t, ":\e[0m ", msg, '\n'
      nErr.inc

    for (p, ob, depth) in t.nodes:
      let q = p[]
      if q.nO == 0:      t.err(depth, "nOb == 0")
      if q.nO > 2*m - 1: t.err(depth, $q.nO & " obs")
      when K isnot void:
        for i in 0.Ix ..< q.nO-1:               # CHECK IN-NODE ORDERING
          if q.ob[i+1].getKey < q.ob[i].getKey:
            t.err(depth, "[" & $(i+1) & "] < [" & $i & "]")
      when Pos isnot void:
        var w = 0                                 # CHECK WEIGHT MAINTENANCE
        when ObSize isnot void:
          for i in 0.Ix ..< q.nO: w += int(q.ob[i].size)
        else:
          w = int(q.nO)
        for i in 0.Ix ..< q.nL: w += q.ln[i][].wt
        if w!=q.wt: t.err(depth,"WEIGHT " & $q.wt & " != self+sum(kids) " & $w)
      if depth == 0:                              # ALMOST DONE IF ROOT
        if q.nL > 0:  
          if   q.nL<q.nO+1:t.err(depth, $q.nL & " L < " & $q.nO & " +1 o")
          elif q.nL>q.nO+1:t.err(depth, $q.nL & " L > " & $q.nO & " +1 o")
        if q.nO < 1: t.err(depth, "UNDERFULL ROOT nOb " & $q.nO)
        return
      if q.nO < m - 1: t.err(depth, "UNDER obs " & $q.nO)
      if q.nL > 0:                              # NON-LEAF
        if   q.nL > 2*m   : t.err(depth, "OVER  lnks " & $q.nL)
        if   q.nL < m     : t.err(depth, "UNDER lnks " & $q.nL)
        if   q.nL < q.nO+1: t.err(depth, $q.nL & " L < " & $q.nO & "+1 o")
        elif q.nL > q.nO+1: t.err(depth, $q.nL & " L > " & $q.nO & "+1 o")
      else:                                       # LEAF => DEPTH == HEIGHT
        if height == -1: height = depth
        if depth != height: t.err(depth, "leaf depth not height: " & $height)
    var k {.used.} = 0
    when Pos isnot void:
      var path: Path
      seekMost(t, path, false)
      while true:
        when ObSize isnot void:
          k += int(path[^1].p[].ob[path[^1].i].size)
        else:
          k.inc
        let r = path.rank()
        if r != k: t.err(0, "rank " & $r & " != " & $k)
        path.seekAdj(true)
        if path.len == 0: break
    when Pos isnot void and defined(checkUnique):
      var path, spath: Path       # This test only works for trees w/UNIQUE keys
      t.seekMost(path, 0)
      for k in 0 ..< t[].wt:
        let r = t.seekNth(path, k)
        if path.len == 0: t.err(0, "cannot seek to nth: " & $k)
        let key = path[^1].p[].ob[path[^1].i].getKey
        t.seekKey(spath, key)
        if spath != path: t.err(0, "seekNth-seekKey path mismatch at " & $k)
      for k in 0 ..< t[].wt:
        let r = t.seekNth(path, k)
        if path.len == 0: t.err(0, "cannot seek to nth: " & $k)
        let r2 = path.rank
        if r2 != r + 1:
          t.err(0, "seekNth-rank path mismatch " & $r2 & " != " & $r & " + 1")
    when K isnot void:
      block:
        var path, spath: Path
        t.seekMost(path, false)
        while path.len > 0:
          let key = path[^1].p[].ob[path[^1].i].getKey
          if not t.seekKey(spath, key):
            t.err(0, "key " & $k & " in tree but cannot be sought")
          path.seekAdj(true)
    return nErr
  
  proc stats(t: Ln; nNode, nOb: var int, depth=0): int {.used.} =
    if t == Ln0: nNode = 0; nOb = 0; return 0 # Get height/alloc data
    var path: Path
    t.seekMost(path, false)
    while path.len > 0:
      nOb.inc                               # Count obs
      if path[^1].i + 1 == path[^1].p[].nO: # At the end of a node
        nNode += 1                          # Count nodes
      path.seekAdj true
