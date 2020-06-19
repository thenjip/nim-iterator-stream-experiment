version = "0.1.0"
author = "thenjip"
description = "An attempt at providing a replacement for closure iterators in Nim with an API similar to Java 8 Stream."
license = "MIT"
srcDir = "src"

requires "nim >= 1.2.0"



import std/[options, os, sequtils, strformat, strutils, sugar]



type
  AbsolutePath = string
  RelativePath = string

  Backend {.pure.} = enum
    C
    Cxx
    Js

  InvalidEnvVarValueError = object of CatchableError



func newInvalidEnvVarValueError (
  name: string;
  value: string
): ref InvalidEnvVarValueError =
  InvalidEnvVarValueError.newException(fmt"""{name}="{value}"""")


func findFirst [I, T](a: array[I, T]; predicate: I -> bool): Option[I] =
  result = I.none()

  for i, item in a:
    if i.predicate():
      return i.some()



func projectName (): string =
  const name = "nim_iterator_stream_experiment"

  name



proc moduleDir (): RelativePath =
  srcDir / projectName()


func nimcacheDirName (): string =
  const name = ".nimcache"

  name


func nimcacheDir (): AbsolutePath =
  const dir = projectDir() / nimcacheDirName()

  dir



func backendEnvVarName (): string =
  const name = "NIM_BACKEND"

  name


func nimCmdNames (): array[Backend, string] =
  const names = ["cc", "cpp", "js"]

  names


func nimCmdName (backend: Backend): string =
  nimCmdNames()[backend]


proc readBackendFromEnv (): Option[Backend] {.
  raises: [InvalidEnvVarValueError, ValueError]
.} =
  let envVarName = backendEnvVarName()

  if envVarName.existsEnv():
    let
      envVarValue = envVarName.getEnv()
      backendFound = nimCmdNames().findFirst(b => b.nimCmdName() == envVarValue)

    if backendFound.isSome():
      backendFound
    else:
      raise newInvalidEnvVarValueError(envVarName, envVarValue)
  else:
    Backend.none()



func testTaskDescription (): string =
  func backendChoice (): string =
    nimCmdNames().`@`().foldl(a & '|' & b)

  @[
    fmt"""Build the test suite in "{nimcacheDir()}{DirSep}" and run it.""",
    fmt"""The backend can be specified in the environment variable "{backendEnvVarName()}=({backendChoice()})"."""
  ].foldl(a & ' ' & b)



task test, testTaskDescription():
  func buildGenDir (module: RelativePath; backend: Backend): AbsolutePath =
    let (dir, file) = module.splitPath()

    nimcacheDir() / backend.nimCmdName() / dir / file


  proc buildCmdLineParts (
    module: RelativePath;
    backendSupplier: () -> Option[Backend]
  ): seq[string] =
    func handleJsFlags (backend: Backend): seq[string] =
      if backend == Backend.Js:
        @["-d:nodejs"]
      else:
        @[]

    let
      backend = backendSupplier().get(Backend.C)
      genDir = module.buildGenDir(backend).quoteShell()
      nimCmd = backend.nimCmdName()
      shortOptions = @["-r"] & backend.handleJsFlags()
      longOptions = @[fmt"--nimcache:{genDir}", fmt"--outdir:{genDir}"]

    @[nimCmd].concat(shortOptions, longOptions, @[module])


  withDir moduleDir():
    for file in system.getCurrentDir().walkDirRec(relative = true):
      if file.endsWith(fmt"{ExtSep}nim"):
        file
          .buildCmdLineParts(readBackendFromEnv)
          .foldl(a & $' ' & b.quoteShell())
          .selfExec()



task clean_test, """Remove the build directory of the "test" task.""":
  let buildDir = nimcacheDir()

  if system.existsDir(buildDir):
    buildDir.rmDir()
