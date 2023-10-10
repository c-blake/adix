#!/bin/sh
# nim-pgo build of: wu lfreq wfr wf
export I=/dev/shm/ToTC # totc=Tale Of Two Cities from Project Gutenberg
export i=/dev/shm/totc # $I pre-processed to lower case via tr A-Z a-z
export n=/dev/null # totc=Tale Of Two Cities from Project Gutenberg
tim 'wu<$I>$n' \
    \
    "tr -cs A-Za-z \\\\n<$I|tr A-Z a-z|mawk '{cnt[\$0]++}'>$n" \
    'tr -cs A-Za-z \\n<$I|tr A-Z a-z|lfreq -n-1' \
    \
    'wfr -n-1 <$I' \
    \
    'wf -n-1 -j1 <$I' \
    'wf -n-1 -j2 <$I' \
    'wf -n-1 -j3 <$I' \
    'wf -n-1 -j4 <$I' \
    \
    'wf -n-1 -j1 <$i' \
    'wf -n-1 -j2 <$i' \
    'wf -n-1 -j3 <$i' \
    'wf -n-1 -j4 <$i'

# (5.693 +- 0.031)e-03   wu<$I>$n   # Simpler - less L1 CPU Cache pressure
#
# Now all the same - accumulate counts, but do no processing after that (this
# is the -n-1 trick in the Nim programs):
#  0.010905 +- 0.000016   tr -cs A-Za-z \\n<$I|tr A-Z a-z|mawk '{cnt[$0]++}'>$n
#  (5.840 +- 0.019)e-03   tr -cs A-Za-z \\n<$I|tr A-Z a-z|lfreq -n-1
#
#  (5.9686 +- 0.0055)e-03 wfr -n-1 <$I
#
#  (4.9756 +- 0.0009)e-03 wf -n-1 -j1 <$I
#  (3.842 +- 0.013)e-03   wf -n-1 -j2 <$I
#  (3.760 +- 0.020)e-03   wf -n-1 -j3 <$I
#  (3.75 +- 0.10)e-03     wf -n-1 -j4 <$I # No point in higher j
#
# These "cheat" by using already lower-cased $i as input, to measure the
# impact of MAP_PRIVATE trick.
#  (4.436 +- 0.013)e-03   wf -n-1 -j1 <$i
#  (3.3724 +- 0.0031)e-03 wf -n-1 -j2 <$i
#  (3.297 +- 0.011)e-03   wf -n-1 -j3 <$i
#  (3.2845 +- 0.0071)e-03 wf -n-1 -j4 <$i # No point in higher j
#
# So, basically, `mawk` is not that bad (~2X worse than `lfreq`), mmap input
# is 5.9686/4.9756=1.2X faster, MAP_PRIVATE costs about 1.14X, and almost all
# parallel speed-up comes from the first doubling of L1 storage.
#
# Also of interest is the approximate algorithm using `bu/oft`:
#  0.01674 +- 0.00017      tr -cs A-Za-z \\n<$I|tr A-Z a-z|oft 1 >$n
# Results match exactly on Tale Of Two Cities for top 12.  While `oft` uses much
# less memory, 17ms is also (2.866 +- 0.031)X slower than `lfreq`.  As mentioned
# in README.md, many sketches need VERY steep space cliffs to pay off in time.
# Aggressive `oft -e0.1 -c0.5` severely degrades matches for only 1.3X speed-up.
#
# In conclusion, advice for vocabulary analysis is use `lfreq` w/preprocessing
# { A) you won't do > ~2X better & B) definition of "word" is likely unstable /
# context-specific; Preproc preserves flex for B without losing much since A. }
