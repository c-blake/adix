when defined(nimdoc):
  import adix/althash
  import adix/bist
  import adix/bitop
  import adix/bltab
  import adix/btree
  import adix/ditab
  import adix/lghisto
  import adix/lptabz
  import adix/memutil
  import adix/metab
  import adix/sequint
  #import adix/stat
  import adix/xlang
else:
  {.error: "use `import adix/{module of interest}`".}
