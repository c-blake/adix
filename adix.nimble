# Package
version     = "0.6.5"
author      = "Charles Blake"
description = "An Adaptive Index Library for Nim"
license     = "MIT/ISC"

# Deps
requires    "nim >= 2.0.0"
requires    "cligen >= 1.8.4"
skipDirs    = @[ "tests" ]

# Older Nim must use adix < 0.5.5 & comment out the below `bin`.
bin         = @[
  "util/lfreq",     # Somewhat efficient line frequency calculator
]
