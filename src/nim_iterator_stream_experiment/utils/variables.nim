when defined(cpp):
  import unit

import std/[sugar]



func read* [T](self: var T): T =
  self


when defined(cpp):
  #[
    "var" return types make the compiler generate invalid C++ code.
    Related issue: https://github.com/nim-lang/Nim/issues/10219
  ]#

  proc write* [T](self: var T; value: T): Unit =
    self = value


  proc modify* [T](self: var T; f: T -> T): Unit =
    self.write(self.read().f())
else:
  proc write* [T](self: var T; value: T): var T =
    self = value

    self


  proc modify* [T](self: var T; f: T -> T): var T =
    self.write(self.read().f())



when isMainModule:
  when defined(cpp):
    import call, ignore

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """Reading a "var" after it being written should return the written value.""":
      proc doTest [T](value: T) =
        var sut {.noInit.}: T

        let
          actual =
            when defined(cpp):
              call(
                proc (): T =
                  sut.write(value).ignore()
                  sut.read()
              )
            else:
              sut.write(value).read()
          expected = value

        check:
          actual == expected


      doTest(2)
      doTest("abc")
      doTest(("a", '0'))
