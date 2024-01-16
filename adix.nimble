# Package
version     = "0.5.10"
author      = "Charles Blake"
description = "An Adaptive Index Library for Nim"
license     = "MIT/ISC"

# Deps
requires    "nim >= 2.0.0"
requires    "cligen >= 1.6.18"
skipDirs    = @[ "tests" ]

# Older Nim must use adix < 0.5.5 & comment out the below `bin`.
bin         = @[
  "util/lfreq",     # Somewhat efficient line frequency calculator
]
