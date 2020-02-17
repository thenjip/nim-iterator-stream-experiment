func `as`* [A](a: A; B: typedesc): B =
  a.B


func `as`* [A; B](a: A): B =
  a as B



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
