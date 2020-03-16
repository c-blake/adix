import tabkey, deftab, otset

defTab(OTTab, OTSet, ot)

when isMainModule:
  proc initTab(): auto = initOTTab[uint32, uint8](4)
  include tests/testTab
