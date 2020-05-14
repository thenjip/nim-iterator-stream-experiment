func default* [T](): T =
  T.default()



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
