import std/[sugar]



func read* [T](self: var T): T =
  self


func write* [T](self: var T; value: T): var T =
  self = value
  result = self


func modify* [T](self: var T; f: T -> T): var T =
  self.write(self.read().f())



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
