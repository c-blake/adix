{.push hint[LineTooLong]:off.}  # Tabular readability trumps 80-col niceness
proc cumsum*[T](c: ptr UncheckedArray[T]; n: uint) =
  for i in 1 ..< n:
    c[i] += c[i - 1]

when defined(amd64) and not defined(usePortable): #usePortable nice for timing
  when defined(cpuPrefetch): import cligen/prefetch

  template workToAligned(c, n, i, align: untyped) {.dirty.} =
    i = 1
    while i < n and (cast[uint](c[i].addr) and (align - 1)) != 0:
      c[i] += c[i - 1]
      i.inc

  template workRemainder(c, n, i: untyped) {.dirty.} =
    while i < n:
      c[i] += c[i - 1]
      i.inc

  type m128i {.importc: "__m128i", header: "emmintrin.h".} = object
  proc mm_load(adr: ptr m128i): m128i {.importc: "_mm_load_si128", nodecl, header: "emmintrin.h".}
  proc mm_store(adr: ptr m128i, val: m128i) {.importc: "_mm_store_si128", nodecl, header: "emmintrin.h".}
  proc mm_set1(ch: uint8): m128i {.importc: "_mm_set1_epi8", nodecl, header: "emmintrin.h".}
  proc mm_set1(wd: uint16): m128i {.importc: "_mm_set1_epi16", nodecl, header: "emmintrin.h".}
  proc mm_set1(dw: uint32): m128i {.importc: "_mm_set1_epi32", nodecl, header: "emmintrin.h".}
  proc mm_add_epi32(a, b: m128i): m128i {.importc: "_mm_add_epi32", nodecl, header: "emmintrin.h".}
  proc mm_shuffle_epi32(a: m128i, msk: cint): m128i {.importc: "_mm_shuffle_epi32", nodecl, header: "emmintrin.h".}
  proc mm_add_epi8(a, b: m128i): m128i {.importc: "_mm_add_epi8", nodecl, header: "emmintrin.h".}
  proc mm_add_epi16(a, b: m128i): m128i {.importc: "_mm_add_epi16", nodecl, header: "emmintrin.h".}
  proc mm_slli_si128(a: m128i, n: cint): m128i {.importc: "_mm_slli_si128", nodecl, header: "emmintrin.h".}
  proc mm_shuffle_epi8(a, b: m128i): m128i {.importc: "_mm_shuffle_epi8", nodecl, header: "tmmintrin.h".}

  proc cumsum*(c: ptr UncheckedArray[uint8]; n: uint) =
    var i = n
    workToAligned(c, n, i, 16)                #Loop to next 16B align
    let n64 = i + ((n - i) and not 63'u64)    #Round dn to mult of 64
    var off = mm_set1(c[i - 1])               #Initial off=last c[].
    let msk = mm_set1(15'u8)
    var v0, v1, v2, v3: m128i                 #SSE vecs
    template do16(v, b: untyped) {.dirty.} =
      v = mm_load(cast[ptr m128i](c[b + i].addr))
      v = mm_add_epi8(v, mm_slli_si128(v, 1)) #0+1  1+2  2+3  3+4  4+5  5+6  6+7  7+8  8+9  9+A  A+B  B+C  C+D  D+E  E+F F
      v = mm_add_epi8(v, mm_slli_si128(v, 2)) #0..3 1..4 2..6 3..6 4..7 5..8 6..9 7..A 8..B 9..C A..D B..E C..F D..F E+F F
      v = mm_add_epi8(v, mm_slli_si128(v, 4)) #0..7 1..8 2..9 3..A 4..B 5..C 6..D 7..E 8..F 9..F A..F B..F C..F D..F E+F F
      v = mm_add_epi8(v, mm_slli_si128(v, 8)) #0..F 1..F 2..F 3..F 4..F 5..F 6..F 7..F 8..F 9..F A..F B..F C..F D..F E+F F
      v = mm_add_epi8(v, off)                         #Add in offset
      mm_store(cast[ptr m128i](c[b + i].addr), v)     #Update array
      off = mm_shuffle_epi8(v, msk)                   #off=bcast high elt
    while i < n64:                                    #1-cache line at a time
      when defined(cpuPrefetch): prefetchw(c[i + 64].addr)
      do16(v0,  0)
      do16(v1, 16)
      do16(v2, 32)
      do16(v3, 48)
      inc(i, 64)  #XXX After cache-line loop, could do few more vectorized doX's
    workRemainder(c, n, i)                            #Loop to end

  proc cumsum*(c: ptr UncheckedArray[uint16]; n: uint) =
    var i = n
    workToAligned(c, n, i, 16)              #Loop to next 16B align
    let n32 = i + ((n - i) and not 31'u64)  #Round dn to mult of 32
    var off = mm_set1(c[i - 1])             #Initial off=last c[]
    let msk = mm_set1(0x0F0E'u16)           #s.t. shuffle_epi8 =~ shuffle_epi16
    var v0, v1, v2, v3: m128i               #SSE vectors
    template do8(v, b: untyped) {.dirty.} =
      v = mm_load(cast[ptr m128i](c[b + i].addr))
      v = mm_add_epi16(v, mm_slli_si128(v, 2)) #0+1  1+2  2+3  3+4  4+5  5+6  6+7 7
      v = mm_add_epi16(v, mm_slli_si128(v, 4)) #0..3 1..4 2..5 3..6 4..7 5..7 6+7 7
      v = mm_add_epi16(v, mm_slli_si128(v, 8)) #0..7 1..7 2..7 3..7 4..7 5..7 6+7 7
      v = mm_add_epi16(v, off)                      #Add in offset
      mm_store(cast[ptr m128i](c[b + i].addr), v)   #Update array
      off = mm_shuffle_epi8(v, msk)                 #off=bcast high elt
    while i < n32:                                  #1-cache line at a time
      when defined(cpuPrefetch): prefetchw(c[i + 32].addr)
      do8(v0,  0)
      do8(v1,  8)
      do8(v2, 16)
      do8(v3, 24)
      inc(i, 32)
    workRemainder(c, n, i)                          #Loop to end

  proc cumsum*(c: ptr UncheckedArray[uint32]; n: uint) =
    var i = n
    workToAligned(c, n, i, 16)                    #Loop to next 16B align
    let n16 = i + ((n - i) and not 15'u64)        #Round dn to mult of 16
    var off = mm_set1(c[i - 1])                   #Initial off=last c[]
    var v0, v1, v2, v3: m128i                     #SSE vectors
    const msk = 0xFF.cint
    template do4(v, b: untyped) {.dirty.} =
      v = mm_load(cast[ptr m128i](c[b + i].addr))
      v = mm_add_epi32(v, mm_slli_si128(v, 4))    #0+1     1+2   2+3 3
      v = mm_add_epi32(v, mm_slli_si128(v, 8))    #0+1+2+3 1+2+3 2+3 3
      v = mm_add_epi32(v, off)                    #Add in offset
      mm_store(cast[ptr m128i](c[b + i].addr), v) #Update array
      off = mm_shuffle_epi32(v, msk)              #off=bcast high elt
    while i < n16:                                #1-cache line at a time
      when defined(cpuPrefetch): prefetchw(c[i + 16].addr)
      do4(v0,  0)
      do4(v1,  4)
      do4(v2,  8)
      do4(v3, 12)
      inc(i, 16)
    workRemainder(c, n, i)                        #Loop to end

  proc cumsum*(c: ptr UncheckedArray[uint64]; n: uint) =
    for i in 1 ..< n:   #Could speed this up, but time will never be big cmp to
      c[i] += c[i - 1]  #..moving billions of items around in a counting sort.

proc cumsum*[T](c: var openArray[T]) {.inline.} =
  cumsum(cast[ptr UncheckedArray[T]](c[0].addr), c.len.uint)

proc cumsum*[T](c: ptr T, n: uint) {.inline.} =
  cumsum(cast[ptr UncheckedArray[T]](c), n)

when isMainModule:
  import random, times, stats, cligen
  when not declared(stderr): import std/[syncio, formatfloat]

  proc gen[T](x: var openArray[T]; low, range: int) =
    for i in 0 ..< x.len: x[i] = low.T + rand(range.float).T

  type Kind = enum kU1, kU2, kU4, kU8

  proc doIt[T](k: T, n=9, low=0, range=9, bench=false, minN=5, avgN=5,
               data: seq[uint64]) =
    randomize()
    var x = newSeq[T](n)
    var t = x
    var dtsMin: RunningStat
    for avgIt in 1..avgN:
      var dtMin = float.high
      for minIt in 1..minN:
        if data.len > 0:    #Allow passing specific data for reproducible debug
          for i in 0 ..< x.len: x[i] = data[i].T
        else:
          x.gen low, range
        if not bench: t = x
        let t0 = epochTime()
        x.cumsum
        dtMin = min(dtMin, (epochTime() - t0) * 1e9)
        if not bench:
          var t0 = t
          for i in 1 ..< n:
            t[i] += t[i - 1]
          for i in 1 ..< n:
            if x[i] != t[i]:
              echo "bad cumsum at: ",i," x: ",x[i]," shouldBe: ",t[i]; echo ""
              echo "x0[]: ", t0; echo ""
              echo "t[]: ", t; echo ""
              echo "x[]: ", x
              return
      dtsMin.push dtMin
    echo "n: ", n, " ", $T, " cumsum_ns: ", dtsMin.min, " .. ", dtsMin.max

  proc testTime(kind=kU1, n=256, low=0, range=128, bench=false, minN=9, avgN=9,
                data: seq[uint64]) =
    case kind
    of kU1: doIt(0'u8 , n, low, range, bench, minN, avgN, data)
    of kU2: doIt(0'u16, n, low, range, bench, minN, avgN, data)
    of kU4: doIt(0'u32, n, low, range, bench, minN, avgN, data)
    of kU8: doIt(0'u64, n, low, range, bench, minN, avgN, data)

  dispatch(testTime, cmdName="cumsum")
