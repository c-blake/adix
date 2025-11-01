when not declared(addFloat): import std/formatfloat
import os, strutils, metab, random

randomize()

let num = if paramCount() > 0: parseInt(paramStr(1)) else: 1
let den = if paramCount() > 1: parseInt(paramStr(2)) else: 4
let cnt = if paramCount() > 2: parseInt(paramStr(3)) else: 3*(1 shl 10)
let nTr = if paramCount() > 3: parseInt(paramStr(4)) else: 30
let rob = paramCount() > 4

#echo "USING ", num, '/', den, " and ", cnt, " entries."
for t in 1..nTr:
  var one = initSet[int](numer=num, denom=den, robinhood=rob)
  for i in 1..cnt:
    one.incl rand(1 shl 32)
  echo "Ut: ", one.len.float/one.getCap.float, " St: ", one.depthStats
