#!/bin/sh
# nim-pgo build of: wu lfreq wfr wf
# /t/I is Dickens' Tale Of Two Cities from Project Gutenberg.
tim 'wu</t/I>/n' \
    \
    "tr -cs A-Za-z \\\\n</t/I|tr A-Z a-z|mawk '{cnt[\$0]++}'>/n" \
    'tr -cs A-Za-z \\n</t/I|tr A-Z a-z|lfreq -n-1' \
    \
    'wfr -n-1 </t/I' \
    \
    'wf -n-1 -j1 </t/I' \
    'wf -n-1 -j2 </t/I' \
    'wf -n-1 -j3 </t/I' \
    'wf -n-1 -j4 </t/I' \
    \
    'wf -n-1 -j1 </t/i' \
    'wf -n-1 -j2 </t/i' \
    'wf -n-1 -j3 </t/i' \
    'wf -n-1 -j4 </t/i'

# (5.693 +- 0.031)e-03   wu</t/I>/n   # Simpler - less L1 CPU Cache pressure
#
# Now all the same - accumulate counts, but do no processing after that (this
# is the -n-1 trick in the Nim programs):
#
# 0.010905 +- 0.000016   tr -cs A-Za-z \\n</t/I|tr A-Z a-z|mawk '{cnt[$0]++}'>/n
# (5.840 +- 0.019)e-03   tr -cs A-Za-z \\n</t/I|tr A-Z a-z|lfreq -n-1
#
# (5.9686 +- 0.0055)e-03 wfr -n-1 </t/I
#
# (4.9756 +- 0.0009)e-03 wf -n-1 -j1 </t/I
# (3.842 +- 0.013)e-03   wf -n-1 -j2 </t/I
# (3.760 +- 0.020)e-03   wf -n-1 -j3 </t/I
# (3.75 +- 0.10)e-03     wf -n-1 -j4 </t/I # No point in higher j
#
# These "cheat" by using already lower-cased /t/i as input, to measure the
# impact of MAP_PRIVATE trick.
# (4.436 +- 0.013)e-03   wf -n-1 -j1 </t/i
# (3.3724 +- 0.0031)e-03 wf -n-1 -j2 </t/i
# (3.297 +- 0.011)e-03   wf -n-1 -j3 </t/i
# (3.2845 +- 0.0071)e-03 wf -n-1 -j4 </t/i # No point in higher j
#
# So, basically, `mawk` is not that bad (~2X worse than `lfreq`), mmap input
# is 5.9686/4.9756=1.2X faster, MAP_PRIVATE costs about 1.14X, and almost all
# parallel speed-up comes from the first doubling of L1 storage.
#
# In light of the above, my advice for problems like these vocabulary analyzers
# is use `mawk|lfreq` with preprocessing since A) you won't do much better & B)
# the stability of the definition of a "word" is probably very context-specific.
# (Preprocessors preserve flexibility for B without losing much since A.)
