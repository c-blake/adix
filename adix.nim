when defined(nimdoc):
  import adix/althash {.used.}
  import adix/amoft {.used.}
  import adix/bist {.used.}
  import adix/bitop {.used.}
  import adix/bltab {.used.}
  import adix/btree {.used.}
  import adix/cumsum {.used.}
  import adix/ditab {.used.}
  import adix/lghisto {.used.}
  import adix/lptabz {.used.}
  import adix/memutil {.used.}
  import adix/metab {.used.}
  import adix/nsort {.used.}
  import adix/sequint {.used.}
  #import adix/stat {.used.}
  import adix/tdigest {.used.}
  import adix/xlang {.used.}
else:
  {.error: "use `import adix/{module of interest}`".}
