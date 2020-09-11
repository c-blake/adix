import lptabz
var c = initLPTab[int,int]()
c.inc 2
c.inc 3
c.inc 3
c.inc 2, -1
c.inc 1
echo c
for k,v in c.topByVal(n=1):
  echo k," ",v
