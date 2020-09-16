import metab
var c = initTab[int8,int]()
c.inc 2
c.inc 3
c.inc 4
c.inc 5
c.inc 6
c.inc 7
c.inc 9
c.inc 9
c.editKey 9, 8
for i in 2'i8..8:
  echo i, " ", c[i]

let t = c
echo t.nthPair(6)
let tup = c.nthPair(0)
echo tup[0]
tup[1][] = 9
echo c
