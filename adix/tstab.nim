import tabkey, deftab, tsset

defTab(TSTab, TSSet, ts)

when isMainModule:
  proc initTab(): auto = initTSTab[uint32, uint8](4)
  include tests/testTab
