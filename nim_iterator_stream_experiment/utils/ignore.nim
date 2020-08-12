func ignore* [T](_: T) =
  ## The ``discard`` operator disguised as a function.
  discard



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
