import metab

#import althash
#proc hash(h, salt: Hash): Hash = hashRoMu1(h) xor cast[Hash](salt)

var one = initSet[int]()
for i in 0 ..< (1 shl 23):
  one.incl (i shl 25)

var ds = one.depths
echo ds
echo one.len, "/", one.getCap
echo "Stats: ", one.depthStats
