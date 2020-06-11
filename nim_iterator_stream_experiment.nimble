version = "0.1.0"
author = "thenjip"
description = "An attempt at providing a replacement for closure iterators in Nim with an API similar to Java 8 Stream."
license = "MIT"
srcDir = "src"

requires "nim >= 1.2.0"



import std/[sequtils, strformat, strutils]
from std/os import ExtSep, `/`, quoteShell, splitFile, walkDirRec



type
  AbsolutePath* = string
  RelativePath* = string



template itemType [T](s: seq[T]): typedesc[T] =
  T



func projectName (): string =
  const name = "nim_iterator_stream_experiment"

  name


proc moduleDir (): RelativePath =
  srcDir / projectName()


func nimcacheDir (): AbsolutePath =
  const dir = projectDir() / ".nimcache"

  dir



task test, fmt"""Build the test suite in "{nimcacheDir()}/" and run it.""":
  func buildCmdLineParts (): seq[string] =
    let
      nimCmd = $'c'
      shortOptions = @["-r"]

    @[nimCmd] & shortOptions


  func buildGenDir (module: RelativePath): AbsolutePath =
    let (dir, name, _) = module.splitFile()

    nimcacheDir() / dir / name


  withDir moduleDir():
    const cmdLineParts = buildCmdLineParts()

    for file in system.getCurrentDir().walkDirRec(relative = true):
      if file.endsWith(fmt"{ExtSep}nim"):
        let genDir = file.buildGenDir().quoteShell()

        cmdLineParts
          .concat(
            @[fmt"--nimcache:{genDir}", fmt"--outdir:{genDir}"],
            @[file]
          ).foldl(a & $' ' & b.quoteShell(), "")
          .selfExec()



task clean_test, """Remove the build directory of the "test" task.""":
  let buildDir = nimcacheDir()

  if buildDir.existsDir():
    buildDir.rmDir()
