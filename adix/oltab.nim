import tabkey, deftab, olset

defTab(OLTab, OLSet, ol)

when isMainModule:
  proc initTab(): auto = initOLTab[uint32, uint8](4)
  include tests/testTab
