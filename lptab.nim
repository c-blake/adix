import tabkey, deftab, lpset

defTab(LPTab, LPSet, lp)

when isMainModule:
  proc initTab(): auto = initLPTab[uint32, uint8](4)
  include tests/testTab
