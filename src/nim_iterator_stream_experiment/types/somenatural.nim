type
  SomeNatural* {.explain.} = concept type T
    T is SomeInteger
    when compiles(T.low().Natural):
      T.low().Natural == Natural.low()
    else:
      false



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """Integer types starting at 0 should match the "SomeNatural" concept.""":
      proc doTest (T: typedesc[SomeInteger]) =
        check:
          T is SomeNatural


      doTest(Natural)
      doTest(uint8)
      doTest(uint64)
      doTest(uint)
      doTest(range[0 .. 9])



    test """Integer types that do not start at 0 should not match the "SomeNatural" concept.""":
      proc doTest (T: typedesc[SomeInteger]) =
        check:
          T isnot SomeNatural


      doTest(Positive)
      doTest(int)
      doTest(int8)
      doTest(range[-1 .. 8])
