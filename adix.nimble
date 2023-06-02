# Package
version     = "0.5.5"
author      = "Charles Blake"
description = "An Adaptive Index Library for Nim"
license     = "MIT/ISC"

# Deps
requires    "nim >= 1.2.0"
requires    "cligen >= 1.6.6"
skipDirs    = @[ "tests" ]

bin         = @[
  "util/lfreq",     # Somewhat efficient line frequency calculator
]
