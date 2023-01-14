# Package
version     = "0.4.6"
author      = "Charles Blake"
description = "An Adaptive Index Library for Nim"
license     = "MIT/ISC"

# Deps
requires    "nim >= 1.2.0"
requires    "cligen >= 1.5.37"
skipDirs    = @[ "tests" ]

bin         = @[
  "util/cstats",    # Preserve context/compute column stats filter
  "util/lfreq",     # Somewhat efficient line frequency calculator
]
