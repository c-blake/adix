when not declared(addFloat): import std/formatfloat
import std/[os, strutils, times], adix/[althash, bltab]

template h(x):untyped = (when defined(lessRan): hashRoMu1(x) else: hashNASAM(x))

let nTab = parseInt(paramStr(1))
let mask = parseInt(paramStr(2))

var s = initBLTab(nTab, mask)
var d: seq[int]
for i in 3 .. paramCount(): d.add parseInt(paramStr(i))
let verb = "V".existsEnv
template maybeEcho(x: varargs[untyped]) =
  if verb: echo x

let t0 = epochTime()
for j in d:
  if j > 0:
    let k = h(j) and mask
    if s.containsOrIncl(k): maybeEcho "had ", j
    else: maybeEcho "added ", (j, k)
  elif j < 0:
    let k = h(-j) and mask
    if s.missingOrExcl(k): maybeEcho "did nothing"
    else: maybeEcho "removed ", (-j, k)
let dt = epochTime() - t0

let ds = s.depths
if verb: s.debugDump
echo "dt(s): ", dt
echo "hashLd: ", float(s.len)/float(s.getCap), " ", ds.len, " depths: ", ds
echo paramCount()-2-s.len, '/', paramCount()-2, ". false pos. (if all +)"

# ./bl $[1<<17] $[(1<<26) - 1] {1..$[3<<15]} |tail -n2
#   hashLd: 0.7494 19 depths: @[36541, 25619, 15310, 8917, 5087, 2965, 1708, 934, 486, 272, 174, 82, 56, 36, 17, 13, 5, 1, 2]
#   79/98304. false pos. (if all +); fpr=0.0008; 19*26/8=61.75 < 64B cache line.
#
# A Bloom filter is less space -1.44*lg.0008=14.8 bit/num (57% of 26 bits, 42.7%
# adjusting for 75% hashLd), BUT needs -lg .0008 = 10.3 hash funs ~10 line lds.
# *MANY* would pay a 1/.427=2.34X space increase to get a 10+X speed boost.  In
# fact, unless you somehow know you will be right at a "cache cliff", almost no
# one would not choose to spend 2.3X space for a 10X speed-up.
#
# Aux 2..3-bit counter fields (28..29./26*2.34=2.52..2.61x space) can buy you
# deletes that are mostly reliable OR dup keys could be allowed (as in `lptabz`)
# for full reliability at the cost of longer collision clusters.  (Well, full
# reliability modulo fingerprint collisions..).
#
# Even compared with Cuckoo filters things are 2x faster in the worst case and
# 1.5x faster on average (depending on probability of 2nd lookup being needed).
#
# Of course, one can do other tests, such as a 1e6 insert 29-bit one:
#   ./bl $[1<<21] $[(1<<29)-1] {1..1000000}
#     dt(s): 0.02814388275146484
#     hashLd: 0.4764270782470703 10 depths: @[669828, 237265, 67871, 17965, 4647, 1192, 275, 71, 23, 3]
#     860/1000000. false pos. (if all +)
#   That will take 29*(1<<21) bits or a 7.6 MiB `seq`.
#
# And one can sometimes do better with a *less* random hash, of course at some
# substantial risk that your hash is over-tuned to your key sets:
#   nim c -d:danger -d:lessRan bl
#   ./bl $[1<<21] $[(1<<22)-1] {1..1000000}
#     dt(s): 0.01258683204650879
#     hashLd: 0.476837158203125 2 depths: @[801457, 198543]
#     0/1000000. false pos. (if all +)
#   That will take 22*(1<<21) bits or a 5.7 MiB `seq`.
#
#
# This is all amenable to more formal analysis for those so inclined.  Here is
# an excerpt of an e-mail I wrote in Summer 2001 (unadjusted for slightly better
# Robin Hood Linear Probing):
# ----------------------------------------------------------------------------
# A few nights ago David M raised some nice specific doubts and prompted me
# to do some simple calculations.  The compellingness of Bloom filters seems
# limited, but very well defined.
#
# The executive summary is just this: for small false positive probabilities
# Bloom filters help if you're trading memory against disk accesses, but
# probably not for fast vs. slow memory where "slow" is only 5..8 times higher
# latency.  Some basic math will perhaps clarify the issue.
#
# The short of it is just this:
#   { ^ -> exponentiation[not xor], lg=log base 2 }
#
#   Consider N objects/packet-types/whatever and an M-bit table.
#   Then let a = N / M be the "load".  We have (see, e.g. Knuth)
#     P(false positive) = p = (1 - exp(-k*a))^k
#
#   Solve for a(k,p)=-log(1-p^(1/k))/k, differentiate with respect to k, set
#   it equal to zero, and solve to get k = lg(1/p) as the maximizer of a, or
#   the *minimizer of M*.  I.e., a Bloom filter with either ceil(-lg p) or
#   floor(-lg p) gives the minimum memory usage for a given target p.
#   What is the memory usage?  Substituting back in notice p^(1/k)=1/2 and:
#     a = -log(1-1/2)/lg(1/p) = log 2 / lg(1/p), or
#     M = -lg e * N * lg p = 1.44*N*lg(1/p)
#
# Compare this with recording "existence" in a hash table of B-bit values.
# Suppose we manage collisions with open-addressed linear probing (a cache
# friendly thing).  To achieve 2 table accesses/query (probably 1 slow memory
# access) we need the load to be ~70% (see Knuth 6.4 table 4).  Specifically,
# M' = 1.44*N slots = 1.44*N*B bits.  Anything in the address space does get
# stored in the table.  So the false positive rate we expect is the collision
# rate in the B-bit address space for N objects.
#
# Standard binomial birthday simplification of multinomial collision analysis is
#     p = 1 − (1 − 1/M')^(N-1), or as M,N get big
#     p = 1 - exp(N*log(1 - 1/M')) =~ 1 - exp(-N/M') = 1 - exp(-N/2^B) or
#     B = lg(-N/log(1-p)).  And so,
#     M' = 1.44*N*lg(-N/log(1-p)) bits.
# Now if p << 1, again using log(1-p) =~ -p { err =~ .5*p^2 < 5% for p < .1 }
#     M' = 1.44*N*lg(N/p).
#
# So there you have it.  The ratio of storage needed for M' (hash table) over
# M (Bloom filter) simplifies for "small" p to (with log_1/p == log base 1/p):
#
#     M'/M = lg(N/p) / lg(1/p) = (lg(1/p) + lg N)/log(1/p) =
#          = 1 + lg N/lg(1/p) = 1 + log_1/p (N) = 1 + lg N/-lg p
#
# This tells you exactly what you need to know -- Bloom filters save space only
# when N is very large relative to 1/p.  E.g., N=1e6 and p=1% give M' = 4M.
# This may surprise as a naive perception may be that you want to Bloom when you
# want small false positive rates.  However, for low p, this costs a lot of time
# as you end up with many hash functions also probing memory randomly.
#
# In time, Bloom only pays off when you are near enough a cliff in latency of
# the memory hierarchy where `k` Bloom accesses beat the 1 LinearProbe access
# because the `k` can operate in region (1+lgN/-lg p)X smaller.
