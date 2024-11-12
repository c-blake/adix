## The min-count sketch (NOT count-min) idea is to see hash(x) as a U(0,1) & use
## P(sampleMax<x)=x^n for sample size n.  Low art inverts a confidence.ival for
## h.max to estimate n.  Tracking `k`-most distinct h gives better accuracy and
## is usually called a KMV sketch. (Intuition is that k-th edge val => *average*
## gap between k-1 uniques&averaging cuts noise.) See Bar-Yossef 2002 "Counting
## Distinct..", Giroire05 "Order statistics & estimating cardinalities" & Ting14
## "Streamed approximate counting..".
# NOTE: Speed-accuracy-space trade-off triangle.  To keep `lowerBound` & rare
# `moveMem` fast, want `tail` cached, meaning small `k` & ~big expected error.
# TODO Augment to AKMV for multiset queries & "cardinality one stop shopping".
# TODO Add a B-Tree for when k>>1000 & folks care about "perf during warm up"
# (As-is is very simple and not slow post warm-up; uniqce 100000 1024 16383 1000
# =~ 2 sec for 100e6 => 20 ns/update.  Better than 4x boost seems v.unlikely.)

const axUCEdefaultK {.intdefine.} = 1024 # -d:axUCEdefaultK=X edits unInit val.
# avg|err| <+-~0.5..1.8%, avgMax1000|err|<~2..8.0% (dep on dups).
import std/[algorithm, hashes, math, sets]
type                    # NOTE: Want no dups & fast del reorg if seq is replaced
  UniqCe*[F:SomeFloat] = object
    tail: seq[F]        # Sorted seq of `k` biggest `Hash`; Q: Add B-Tree mode?
    k: int              # State scale & lower bound of true answer
    est: float64        # Running estimate a la Ting2014

proc initUniqCe*[F:SomeFloat](k=1024): UniqCe[F] = result.k = k
  ## Return initialized `UniqCe` with tail size `k`.  k=1024 costs 4K|1VM page

proc push*[F:SomeFloat](uc: var UniqCe[F], h: F) =
  ## Incorporate visitation of an element.  NOTE: std/hashes.hash(string) OUTPUT
  ## ONLY FILLS 32-BITS => push takes `x on [0,1]` instead of a key|hash() val.
  if uc.k == 0: uc.k = axUCEdefaultK            # Make ok w/`var x: UniqCe`
  if uc.tail.len == 0:                          # Initial empty check
    uc.tail.add h.F                             # Branch-predicted out
    return
  if uc.tail.len == uc.k and h <= uc.tail[0]:   # No change to tail
    return # Most activity for large inputs ends here post-warm-up;ByDef("tail")
  let i = uc.tail.lowerBound(h)                 # BINARY SEARCH for ins. spot
  if i < uc.tail.len and uc.tail[i] == h:       # Already in tail; !DUPS=>done
    return
  if uc.tail.len < uc.k:                        # BUILD PHASE; always insert
    uc.tail.insert h, i
    uc.est += 1.0/float64(1.0 - uc.tail[0])
  elif h > uc.tail[0]:                          # RARE: TAIL GETS NEW ELT
    if i > 1:                                   # i >= 1: must make room
      moveMem uc.tail[0].addr, uc.tail[1].addr, (i - 1)*uc.tail[0].sizeof
    uc.tail[i - 1] = h                          # i is pre-downshift spot
    uc.est += 1.0/float64(1.0 - uc.tail[0])

proc nUnique*[F:SomeFloat](uc: UniqCe[F]): float32 =
  ## Estimate number of unique elements seen so far.
  if uc.tail.len < uc.k: uc.tail.len.float32 else: max(uc.k.float32, uc.est)

proc nUniqueErr*[F:SomeFloat](uc: UniqCe[F]): float32 =
  ## Estimated error on estimate of unique elements seen so far.
  if uc.tail.len<uc.k: 0.float32
  else: uc.nUnique/sqrt(uc.k.float32 - 1) #XXX Est better

proc jaccard*[F:SomeFloat](a: UniqCe[F], b: UniqCe[F]): float32 =
  ## Exact Jaccard coefficient (`|intersect|/|union|`) of the two sample tails.
  ## NOTE: `jaccard(a,b) * union(a, b).nUnique` estimates `nUnique(a ^ b)`.
  var s: HashSet[F]
  for e in a.tail: s.incl e
  var i = 0
  var u = a.len
  for e in b.tail:
    if e in s: inc i    # e in both a & b
    else: inc u         # OR e in just b
  float32(i.float/u.float)

proc union*[F:SomeFloat](a: var UniqCe[F], b: UniqCe[F]) =
  ## Push all hashes from `b` onto `a` effecting an estimated set union.
  for e in b.tail: a.push e

proc union*[F:SomeFloat](a: UniqCe[F], b: UniqCe[F]): UniqCe[F] =
  ## Return merge/union of `a` & `b`.
  result = a; a.union b

when isMainModule:  # Takes numberOfItems/trial, stateSize, dupMask, numTrials
  when not declared(addFloat): import std/formatfloat
  import std/[random, strutils, os, stats]; randomize()
  let n = if paramCount() > 0: parseInt(paramStr(1)) else: 100000
  let k = if paramCount() > 1: parseInt(paramStr(2)) else: axUCEdefaultK
  let d = if paramCount() > 2: parseInt(paramStr(3)).uint64 else: 0xFFFFFFFF'u64
  let m = if paramCount() > 3: parseInt(paramStr(4)) else: 1000
  let s = paramCount() > 4  # skip tracking HashSet error if present
  var err, nSt, eer: RunningStat
  for i in 1..m:
    var uc = initUniqCe[float32](k)
    var st = initHashSet[uint64](n)
    for j in 1..n:
      let key = (randState.next and d)
      uc.push float32(cast[uint64](hash(key)))*(1.0/1.8446744073709551615e19)
      if not s: st.incl key
    let est = uc.nUnique
    if not s:
      let e = abs(est - st.len.float)/st.len.float
      err.push e; eer.push uc.nUniqueErr/est/e
    nSt.push est
  echo nSt.mean, " ", err.mean*100, " % ", err.max*100, " % ", eer.mean
