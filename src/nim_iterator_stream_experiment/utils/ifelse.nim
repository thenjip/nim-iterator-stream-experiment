import std/[sugar]



proc ifElse* [T](condition: bool; then: () -> T; `else`: () -> T): T =
  if condition:
    then()
  else:
    `else`()



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
