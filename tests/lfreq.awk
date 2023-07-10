#!/bin/awk -f
{ cnt[$0]++ }
END { for(k in cnt) print cnt[k], k }

# As has surely been noted about a gajillion times by now, an improvement on
# Doug McIlroy's solution in Knuth-McIlroy is (probably):
#   tr -cs A-Za-z \\n | # Non-alpha -> newline; NOTE: apostrophe-quoting ambig.
#     tr A-Z a-z      | # ASCII upper to lower
#     lfreq.awk       | # Histogram lines (this script)
#     sort -n | tail    # top 10; Add `| tac` if you like decreasing
#
# While almost any timing strongly depends on used vocab & its sampled growth,
# I get `mawk` 2..3X slower than optimized Nim & `gawk` ~2X slower than `mawk`.
#
# Given enough CPU cores, all above stages run in parallel & execution time is
# bounded by pipe BW & the slowest stage - likely this AWK script.  McIlroy's
# `sort|uniq -c` method may be better if unique lines exceed avail. phys RAM &
# next level of mem hierarchy has high rand.access latency (eg. Winchester,net).
