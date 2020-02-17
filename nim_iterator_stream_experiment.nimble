# Package

version       = "0.1.0"
author        = "thenjip"
description   = "An attempt at providing a replacement for iterators in Nim with an API similar to Java 8 Stream."
license       = "MIT"
srcDir        = "src"



# Dependencies

requires "nim >= 1.0.6"



import std/[os, sequtils, strformat, strutils]



task test, "":
  withDir srcDir:
    let
      projectName = "nim_iterator_stream_experiment"
      cmdElements =
        [
          "c",
          "-r",
          projectDir() / srcDir / fmt"{projectName}{ExtSep}nim"
        ]

    cmdElements.foldl([a, b.quoteShell()].join($' '), "").selfExec()
