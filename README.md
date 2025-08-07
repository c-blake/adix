While this began just as a kind of adaptive index/hash table library, it has
grown into more a collection in the theme of database/big-data related data
structures & algorithms.  { Let's say the "ad" in "adix" now stands for
"ADvanced" | "AscenDant" as well as "adaptive" ;-) }  Most of these are *à la
carte* and I hope you find them useful.  I try to keep the source code short &
to the point.  In particular, as an overview/index, here be:

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

 - Basic Sketches (abbreviated/approximate stores; aka "Digests") for:
   - Membership: [bltab](https://c-blake.github.io/adix/adix/bltab.html) (bit-
   level table; Like more successfully marketed Bloom|Cuckoo filters, but lower
   latency & slightly bigger)
   - Count Distinct: [uniqce](https://c-blake.github.io/adix/adix/uniqce.html)
   aka count unique or cardinality estimation
   - Approx Most Often: [amoft](https://c-blake.github.io/adix/adix/amoft.html)
   (aka approximate top-K most frequent | heavy-hitters)

   - Distributions/Quantiles:
      * [tdigest](https://c-blake.github.io/adix/adix/tdigest.html) (for
        slower, more accurate in tail only quantiles (generalization of medians).
      * for a more complete & adaptive picture, you want accuracy-everywhere /
        full histograms able to realize fast moving quantile (e.g. moving
        median) transforms backed by possibly [Fenwick/BIST
        trees](https://c-blake.github.io/adix/adix/bist.html) with time kernels
        that are flat, [linear](https://c-blake.github.io/adix/adix/lmbist.html)
        or [exponential](https://c-blake.github.io/adix/adix/embist.html).
      * Optional discretizing for binning via logs/sqrt/whatever values gives
        [lghisto](https://c-blake.github.io/adix/adix/lghisto.html), a high
        dynamic range (HDR) module that handles that one-stop shopping style.
   - An amalgam: [`mvstat`](https://c-blake.github.io/adix/adix/mvstat.html)
   that works like `std/stats` but supports `del`, i.e. sliding/moving windows
   over data streams (like a moving average) as well as running|moving quantiles
   via `lghisto`.  Also includes bulk array stats that in some compile modes get
   fully SIMD vectorized inner loops.

And some utility modules:
  - [althash](https://c-blake.github.io/adix/adix/althash.html): salt-able
  alternate hash functions for lptabz
  - [xlang](https://c-blake.github.io/adix/adix/xlang.html): Definitions for C
  refugees for bist
  - [sequint](https://c-blake.github.io/adix/adix/sequint.html): a fixed stride
  "bit matrix" using "batch"/number ops.
  - [memutil](https://c-blake.github.io/adix/adix/memutil.html): memory shifting
  utilities
  - [cumsum](https://c-blake.github.io/adix/adix/cumsum.html): parallel prefix
  sum using Intel SIMD for nsort
  - [bitop](https://c-blake.github.io/adix/adix/bitop.html): re-impl std/bitops
  things to be more CT friendly
  - [topk](https://c-blake.github.io/adix/adix/topk.html): spends 2X the (small)
  space of `std/heapqueue`-based top-k stream algo to scale O(lg k) better via
  what one might call "buffered quickselect".
  - [lna](https://c-blake.github.io/adix/adix/lna.html): natural log(abs(x))
  approximation more tunable with regard to speed-precision trade-offs with
  5-levels of work (3..11 series terms)/precision (11..24 bits).  It's not
  always faster, but is more reliably fast and more tunable than libc using 2
  simple ideas: IEEE exponent to narrow problem & two series for the remaining
  near-unity interval.

A Word/Paragraph Of Caution
===========================
While sketches are popular, like merge sort (vs. radix sort), they often need
huge data to pay off.  Essentially, probabilistic analysis ("Oh wow, I can do
that?!") distracts from more practical space-time trade-offs.  This distraction
is worsened by there being space-time-accuracy "trade-off pyramids".  This
results in an odd state of affairs where I can say here "spending *a bit* more
space can yield major speed-ups", and it sounds blatantly obvious to even the
most casual observer.  *Yet* such is also neglected in context countless times.
The academic literature does not help, often being "blood sport" for more
compressed data | accuracy with no regard to speed.[^1]

So, e.g., on my primary 32 GiB RAM dev box with `bu/zipf`, I cannot make exact
`lfreq` slower than Approximately Most Often sketches(`bu/oft`).  `tests/bl.nim`
shows another example (also written up
[here](https://blog.cloudflare.com/when-bloom-filters-dont-bloom/) in a Bloom
filter / membership approximation context where spending 2-4X what a Bloom takes
space-wise can buy a 7-10X latency shrink.[^2] (Histograms & UCE are both pretty
good deals, though, if errors are acceptable, and `adix/bltab` with fingerprint
keys is arguably just "a better 'sketch' ").

A little more on LPTabz & friends
=================================
As a brief guide I would start with `NOTES.md` and then look at the top part of
`lptabz.nim`.  `TODO.md` also has a lot of notes in it.  My overarching vision
is to allow "the fast way" most of the time, especially for developers that know
how to provide a good `hash`, but to also have auto fall backs to "safer ways"
with optional messages to let devs know they may need to intervene by changing
some defaults at table construction time (or else let users/sysadmins know that
some input may be violating the assumptions of some code sensitive to inputs).
Commercial database systems may have done this for decades, but hasn't really
percolated into commonly available runtime libs.  (Depth-based growth trigger is
likely the simplest example of Profile-Guided Optimization for data structures.
A.Dain Samples 1993 PhD thesis has some more.)

[^1] The basic issue seems to be a need for apparent novelty over practicality
to interest peer reviewers.  Besides weak motivation, "expert" committees only
have whatever narrow exposure they have to various domains of ideas/assumption
frameworks.  With human psychology & incentives this leads to research fads/gobs
of work & "many new names for old ideas" to sift through in deep dives.  One
hoped search engines/LLMs might have eased such sifting, but it seems harder,
perhaps because [synonym density](https://github.com/c-blake/thes) is simply too
great and more folks are at work.

[^2] Note that hardware memory systems got more sophisticated about speculative
workahead execution and parallel fetch which can mask most or all of the extra
latency in a hot loop benchmark, but this is still "more work/mem bandwidth"
competing with other work you *might* want a CPU to be doing instead and The
Memory Wall has been around for like 30 years now.
