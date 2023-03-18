While this began just as a kind of adaptive index/hash table library, it has
grown into more a collection in the theme of database/big-data related data
structures & algorithms.  { Let's say the "ad" in "adix" now stands for
"ADvanced" or "AscenDant" now instead of "adaptive" ;-) }  Most of these are
just kind of *à la carte* and I hope you find them useful.  I trie to keep the
source code short & to the point.  In particular, as an overview/index here be:

 - The original associative lookup modules:
   - [ditab](https://c-blake.github.io/adix/adix/ditab.html): direct-indexed
   table for small universe keys
   - [lptabz](https://c-blake.github.io/adix/adix/lptabz.html): a hash table
   that should be safe for untrusted key sets
   - [metab](https://c-blake.github.io/adix/adix/metab.html): an instantiation/
   table portability helper for lptabz
   - [btree](https://c-blake.github.io/adix/adix/btree.html): a B-Tree with
   various optional features (histogram, ranks, bulk loading, etc.)

 - Basic Sorting: [nsort](https://c-blake.github.io/adix/adix/nsort.html)
     Radix sort only by NEEDED BITS; Often 5-10X faster than `algorithm.sort` if
     you sort a lot of meso-scale data (merge sorts *always* win for HUGE data;
     Very few have it). (Could buffer writes to ensure full cache-line pokes.)

 - Basic Sketches (abbreviated/approximate stores) for:
   - quantiles: [lghisto](https://c-blake.github.io/adix/adix/lghisto.html) or
   [tdigest](https://c-blake.github.io/adix/adix/tdigest.html) (for slower more
   accurate tail quantiles)
   - count distinct: [uniqce](https://c-blake.github.io/adix/adix/uniqce.html)
   aka count unique or cardinality estimation
   - membership: [bltab](https://c-blake.github.io/adix/adix/bltab.html) (bit-
   level table; Like more successfully marketed Bloom filters or Cuckoo filters,
   but lower latency & slightly bigger)
   - approx most often: [amoft](https://c-blake.github.io/adix/adix/amoft.html)
   (aka approximate top-K most frequent | heavy-hitters)
   - In general, a sketch is a 'data summary' providing approximate answers.
   So, even std/stats.RunningStat technically counts.
   - An assimilation: [`stat`](https://c-blake.github.io/adix/adix/stat.html)
   that works like `std/stats` but supports `del`, i.e. sliding/moving windows
   over data streams (like a moving average) as well as quantiles via `lghisto`.
   Also includes bulk array stats that in some compile modes get fully SIMD
   vectorized inner loops.

 - and a few utility modules:
  - [althash](https://c-blake.github.io/adix/adix/althash.html): an assortment
  of salt-able alternate hash functions for lptabz
  - [xlang](https://c-blake.github.io/adix/adix/xlang.html): little definitions
  for C refugees for bist
  - [sequint](https://c-blake.github.io/adix/adix/sequint.html): a fixed stride
  "bit matrix" for bltab
  - [memutil](https://c-blake.github.io/adix/adix/memutil.html): memory shifting
  utilities for lptabz
  - [cumsum](https://c-blake.github.io/adix/adix/cumsum.html): parallel prefix
  sum using Intel SIMD for nsort
  - [bitop](https://c-blake.github.io/adix/adix/bitop.html): re-impl some
  std/bitops things to be more CT friendly

A little more on LPTabz & friends
=================================
As a brief guide I would start with `NOTES.md` and then look at the top half of
`lptabz.nim`.  `TODO.md` also has a lot of notes in it.  My overarching vision
is to allow "the fast way" most of the time, especially for developers that know
how to provide a good `hash`, but to also have auto fall backs to "safer ways"
with optional messages to let developers know they might want to intervene by
changing some defaults at table construction time (or else let users/sysadmins
know that some input may be violating the assumptions of some code sensitive to
inputs).  I think this may be a thing that commercial database systems have done
for decades, but hasn't really percolated into commonly available runtime libs.
(Depth based growth trigger is probably the simplest example of a profile-guided
optimization for data structures. A.Dain Samples 1993 PhD thesis has some more.)
