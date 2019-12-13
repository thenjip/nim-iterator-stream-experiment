# Package

version       = "0.1.0"
author        = "thenjip"
description   = "An attempt at providing a replacement for iterators in Nim with an API similar to Java 8 Stream."
license       = "MIT"
srcDir        = "src"



# Dependencies

requires "nim >= 1.0.4"



import std/[os, sequtils, strutils]



task test, "":
  let cmdElements = [
    "c",
    "-r",
    projectDir() / srcDir / "nim_iterator_stream_experiment" / "stream.nim"
  ]

  cmdElements.foldl([a, b.quoteShell()].join($' '), "").selfExec()
