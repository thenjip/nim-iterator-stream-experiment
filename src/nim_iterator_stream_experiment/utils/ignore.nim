func ignore* [T](_: T) =
  discard



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
