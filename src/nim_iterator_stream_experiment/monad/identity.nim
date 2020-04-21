import std/[sugar]



func itself* [T](value: T): T =
  value


proc apply* [A; B](self: A; f: A -> B): B =
  self.f()



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
