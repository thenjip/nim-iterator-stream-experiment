import utils/[chain, convert, default, ifelse, ignore, lambda, unit]



export chain, convert, default, ifelse, ignore, lambda, unit



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
