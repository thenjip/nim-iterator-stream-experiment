import std/[strutils]



func stripTrailing (input: string; chars: set[char]): string =
  result = input

  result.removeSuffix(chars)


func nimbleProjectName (): string =
  projectName().stripTrailing(Digits).stripTrailing({'_'})



version = "0.1.0"
author = "thenjip"
description = "An attempt at providing a replacement for closure iterators in Nim with an API similar to Java 8 Stream."
license = "MIT"

requires "nim >= 1.2.0"

installDirs = @[nimbleProjectName()]



import nimble/[envvar, paths]

import std/[options, os, sequtils, strformat]



func nimcacheDirName (): string =
  ".nimcache"


func nimcacheDir (): AbsoluteDir =
  projectDir() / nimcacheDirName()



func testTaskDescription (): string =
  func backendChoice (): string =
    nimBackendShellValues().`@`().foldl(a & '|' & b)

  @[
    fmt"""Build the tests in "{nimcacheDir()}{DirSep}" and run them.""",
    "The backend can be specified with the environment variable " &
      fmt""""{EnvVar.NimBackend.shellName()}=({backendChoice()})"."""
  ].foldl(a & ' ' & b)



task test, testTaskDescription():
  func buildBaseOutDir (module: RelativeFile; backend: Backend): AbsoluteFile =
    nimcacheDir() / backend.shellValue() / module


  func binOutDirName (): string =
    "bin"


  func handleJsFlags (backend: Backend): seq[string] =
    if backend == Backend.Js:
      @["-d:nodejs"]
    else:
      @[]


  proc buildCmdLineParts (module: RelativeFile; backend: Backend): seq[string] =
    let
      nimCmd = backend.nimCmdName()
      baseOutDir = module.buildBaseOutDir(backend)
      binOutDir = baseOutDir / binOutDirName()
      options =
        @["-r"].concat(
          backend.handleJsFlags(),
          @[
            fmt"--nimcache:{baseOutDir.quoteShell()}",
            fmt"--outdir:{binOutDir.quoteShell()}"
          ]
        )

    @[nimCmd].concat(options, @[module.quoteShell()])


  withDir nimbleProjectName():
    for file in system.getCurrentDir().walkDirRec(relative = true):
      if file.endsWith(fmt"{ExtSep}nim"):
        file
          .buildCmdLineParts(readNimBackendFromEnv().get(Backend.C))
          .foldl(a & $' ' & b)
          .selfExec()



task clean_test, """Remove the build directory of the "test" task.""":
  let buildDir = nimcacheDir()

  if system.existsDir(buildDir):
    buildDir.rmDir()
