import monad/[identity, io, optional, reader, state]



export identity, io, optional, reader, state



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
