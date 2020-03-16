import tabkey, deftaz, ilset

defTab(ILTab, ILSet, il)

when isMainModule:
  proc initTab(): auto = initILTab[uint32, uint8, 0](4)
  include tests/testTab
