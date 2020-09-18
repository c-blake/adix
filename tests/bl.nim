import althash, bltab, os, strutils

let nTab = parseInt(paramStr(1))
let mask = parseInt(paramStr(2))

var s = initBLtab(nTab, mask)
for i in 3 .. paramCount():
  let j = parseInt(paramStr(i))
  if j > 0:
    let k = hashNASAM(j) and mask
    if s.containsOrIncl(k): echo "had ", j
    else: echo "added ", (j, k)
  elif j < 0:
    let k = hashNASAM(-j) and mask
    if s.missingOrExcl(k): echo "did nothing"
    else: echo "removed ", (-j, k)

let ds = s.depths
s.debugDump
echo "hashLd: ", float(s.len)/float(s.getCap), " ", ds.len, " depths: ", ds
echo paramCount()-2-s.len, '/', paramCount()-2, ". false pos. (if all +)"

# ./bl $[1<<17] $[(1<<26) - 1] {1..$[3<<15]} |tail -n2
#   hashLd: 0.7494 19 depths: @[36541, 25619, 15310, 8917, 5087, 2965, 1708, 934, 486, 272, 174, 82, 56, 36, 17, 13, 5, 1, 2]
#   79/98304. false pos. (if all +); fpr=0.0008; 19*26/8=61.75 < 64B cache line.
#
# A Bloom filter is less space -1.44*lg.0008=14.8 bit/num (57% of 26 bits, 42.7%
# adjusting for 75% hashLd), BUT needs -lg .0008 = 10.3 hash funs ~10 line lds.
# *MANY* would pay a 1/.427=2.34X space increase to get a 10+X speed boost.  Aux
# 2..3-bit counter fields (28..29./26*2.34=2.52..2.61x space) could be used for
# deletes that are mostly reliable OR dup keys could be allowed (as in `lptabz`)
# for full reliability at the cost of longer collision clusters.  Even compared
# with Cuckoo filters things are 2x faster in the worst case and 1.5x faster on
# average (depending on probability of a 2nd lookup being needed).  (Well, full
# reliability modulo fingerprint collisions..).
