# Package
version     = "0.5.8"
author      = "Charles Blake"
description = "An Adaptive Index Library for Nim"
license     = "MIT/ISC"

# Deps
requires    "nim >= 2.0.0"
requires    "cligen >= 1.6.15"
skipDirs    = @[ "tests" ]

bin         = @[
  "util/lfreq",     # Somewhat efficient line frequency calculator
]
