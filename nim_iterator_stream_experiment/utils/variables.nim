##[
  Utilities to manipulate variables.

  Useful when composing procedures that use variables with others that do not.
]##



import std/[sugar]



func read* [T](self: var T): T =
  self


proc write* [T](self: var T; value: T): var T =
  self = value

  #[
    Workaround from
    https://github.com/nim-lang/Nim/issues/10219#issue-396282902 .
  ]#
  when defined(cpp):
    (addr self)[]
  else:
    self


proc modify* [T](self: var T; f: T -> T): var T =
  self.write(self.read().f())



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """Reading a "var" after it being written should return the written value.""":
      proc doTest [T](value: T) =
        var sut {.noInit.}: T

        let
          actual = sut.write(value).read()
          expected = value

        check:
          actual == expected


      doTest(2)
      doTest("abc")
      doTest(("a", '0'))
