import tabkey, deftab, diset

defTab(DITab, DISet, di)

when isMainModule:
  proc initTab(): auto = initDITab[uint32, uint8](4)
  include tests/testTab
