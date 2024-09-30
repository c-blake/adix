when defined(nimdoc):
  import adix/althash
  import adix/amoft
  import adix/bist
  import adix/bitop
  import adix/bltab
  import adix/btree
  import adix/cpuCT
  import adix/cumsum
  import adix/ditab
  import adix/lghisto
  import adix/lptabz
  import adix/memutil
  import adix/metab
  import adix/mvstat
  import adix/nsort
  import adix/oats
  import adix/sequint
  import adix/stat
  import adix/tdigest
  import adix/topk
  import adix/uniqce
  import adix/xlang
else:
  {.error: "use `import adix/{module of interest}`".}
