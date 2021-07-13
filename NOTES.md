These notes are about lptabz/LPTabz.

Unlike most hash table libraries, we do not use load factor to *guess* when
growth *might* be necessary to preserve performance (conditioned upon good hash
functions relative to the key set).  Instead, we measure probe sequence depth
during insert and grow only if it is too deep (or some small `minFree` limit is
hit).  "Too long" is `> numer/denom*lg(size)` since the worst per table case on
random data scales that way.  This approach is both more robust to weakly
scrambling hashes and more space conservative for "better than random" hashes.
It also fixes this problem:
  https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion

However, we also want to avoid memory exhaustion.  So, we only grow tables if
not "too sparse", i.e. `count/length > 1/(1 shl growPo2)`.  For `growPo2=1` this
means we might still double at only 50% full taking it down to 25%.  If 25-50%
load cannot give expected collision cluster sizes then A) the hash function is
inadequate, B) the table is under attack, or C) it is being abused with many
duplicate keys.  The first two of these situations are basically the same with
a natural response - use a more scrambling hash function with hard to guess data
mixed in.  Ignorance of the underlying keys only allows us to do that "on top"
of the `hash` the user already provides which is what we do here.  This will be
ineffective if that provided hash outputs too few hash code values.  Our re-hash
of the hash mixes in the VM address of its data area, unique to each table and
table size.  We usually emit a warning when activating this feature, though that
can be disabled.

This resize protocol makes performance much more deterministic, but also makes
space utilization non-deterministic.  Utilization can be both much better than
typical load-based resize with a near perfect hash as well as a little worse
with a too weak hash.  This seems "how it should be".  Safer performance also
seems worth more than deterministic size, and you cannot have both at once with
an abstract key and user-settable hash.  I'm a bit surprised this resize
protocol isn't more popular.

If you want to monitor space utilization you can do `.len/.getCap`.  The tables
here also all provide a query function `depths` to inspect distribution of probe
depth.  `depths` is about as expensive to compute as looking each item up once.
It can be a bit faster on some key types.  More general performance forensics,
are available if `hashStats` is defined to count various important events.
That activates counters for a variety of events like probes, different resize
conditions and so on.  Each counter just starts at zero and goes up.  So, you
just use them a bit like `epochTime`/`getTime` and friends.  You can "time" at
whatever granularity is desired..  They are just global variables and so not
exactly multi-thread safe, but the worst that can happen under MT contention is
that you loose a few counts.  { E.g., A) load old val, B) load old val, both
inc, only one writes back.  So, you get 1 inc instead of 2. }

Some OSes can accelerate context switches if FP registers are never dirtied.
So, this library is careful to avoid floating point with integer ratios.
It is also careful to avoid high pipeline latency integer divisions.

These tables also allow shrinkage in case many deletes have occurred and more
than one iteration might later be performed. { A `setCap` is slower than one
iteration.  So, if it's only 1 more there is no point. } The default new size
parameter (or an explicit negative one) causes the table to grow by one standard
expansion while zero (or any small positive number) will make it be compacted
to the minimum size that can fit its current population (possibly plus a fudge
factor based on hash randomness assumptions for tables that need that).

A perhaps non-obvious subtlety about Robin Hood hashing with depth-triggered
growth is that `pushUp` can increase depth at the end of the collision cluster.
That is the depth we want tested in `tooFull`.  So, `rawPut` is two phases for
RH.  `rawPut1` finds the end of a collision cluster.  `rawPut2` does the actual
shift.  In between, we can call `tooFull` to see if resizing is necessary.
Note that the depth `rawPut1` calculates is actually the "shift size" not the
max search depth of moved elements.  This is the sense of depth you want since
the point of depth-triggered resize is to avoid both large scans and large data
motion. { "Cost" might be a more clear word than "depth". }
