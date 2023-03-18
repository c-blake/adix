when defined(nimdoc):
  import adix/althash
  import adix/amoft
  import adix/bist
  import adix/bitop
  import adix/bltab
  import adix/btree
  import adix/cumsum
  import adix/ditab
  import adix/lghisto
  import adix/lptabz
  import adix/memutil
  import adix/metab
  import adix/nsort
  import adix/sequint
  #import adix/stat
  import adix/tdigest
  import adix/xlang
else:
  {.error: "use `import adix/{module of interest}`".}
