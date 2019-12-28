func itself* [T](value: T): T =
  value



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "itself: various types":
      proc itselfTest [T](expected: T) =
        let sut = expected.itself()

        check:
          sut == expected


      itselfTest(outOfMemHook.unsafeAddr())
      itselfTest(("a", 0, 0.0))
      itselfTest(new IOError)
      itselfTest('a')
