import tabkey, deftab, ssset

defTab(SSTab, SSSet, ss)

when isMainModule:
  proc initTab(): auto = initSSTab[uint32, uint8]()
  include tests/testTab
