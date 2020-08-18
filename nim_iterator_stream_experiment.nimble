version = "0.1.0"
author = "thenjip"
nimscriptapi.description =
  "An attempt at providing a replacement for closure iterators in Nim with an API similar to Java 8 Stream."
license = "MIT"

requires "nim >= 1.2.0"



import std/[macros, options, os, sequtils, strformat, strutils, sugar]



# Paths

type
  AbsoluteDir = string
  AbsoluteFile = string

  RelativeDir = string
  RelativeFile = string



func nimcacheDirName (): string =
  ".nimcache"


func nimcacheDir (projectDir: AbsoluteDir): AbsoluteDir =
  projectDir / nimcacheDirName()



# Utils

func findFirst [I, T](a: array[I, T]; predicate: T -> bool): Option[I] =
  result = I.none()

  for i, item in a:
    if item.predicate():
      return i.some()



func stripTrailing (input: string; chars: set[char]): string =
  result = input

  result.removeSuffix(chars)


func stripTrailing (input: string; c: char): string =
  input.stripTrailing({c})



func joinWithSpace (a: openArray[string]): string =
  a.join($' ')



iterator relativeNimModules (dir: AbsoluteDir): RelativeFile =
  for file in dir.walkDirRec(relative = true):
    if file.endsWith(fmt"{ExtSep}nim"):
      yield file



# Nim command line

func toNimLongOption (name, value: string): string =
  fmt"--{name}:{value.quoteShell()}"



func toNimLongOptions (
  pairs: openArray[tuple[name, value: string]]
): seq[string] =
  pairs.map(pair => pair.name.toNimLongOption(pair.value))



# Environment variables

type
  EnvVar {.pure.} = enum
    NimBackend

  Backend {.pure.} = enum
    C,
    Cxx,
    Js

  InvalidEnvVarValueError = object of CatchableError



func shellNames (): array[EnvVar, string] =
  const names = ["NIM_BACKEND"]

  names


func shellName (self: EnvVar): string =
  shellNames()[self]



func nimBackendShellValues (): array[Backend, string] =
  const shellValues = ["c", "cxx", "js"]

  shellValues


func shellValue (self: Backend): string =
  nimBackendShellValues()[self]



func nimBackendNimCmdNames (): array[Backend, string] =
  const cmdNames = ["cc", "cpp", "js"]

  cmdNames


func nimCmdName (self: Backend): string =
  nimBackendNimCmdNames()[self]



func newInvalidEnvVarValueError (
  name: string;
  value: string
): ref InvalidEnvVarValueError =
  InvalidEnvVarValueError.newException(&"{name}=\"{value}\"")


func newInvalidEnvVarValueError (
  self: EnvVar;
  value: string
): ref InvalidEnvVarValueError =
  newInvalidEnvVarValueError(self.shellName(), value)



proc readFromEnv [T](
  self: EnvVar;
  fetchEnvValue: (shellName: string) -> Option[string];
  parseValue: (envValue: string) -> T
): Option[T] =
  self.shellName().fetchEnvValue().map(parseValue)



func tryParseBackend (value: string): Option[Backend] =
  nimBackendShellValues().findFirst(shellValue => value == shellValue)


proc readNimBackendFromEnv (
  fetchEnvValue: (shellName: string) -> Option[string]
): Option[Backend] =
  const envVar = EnvVar.NimBackend

  func parseBackendOrRaise (envValue: string): Backend =
    let opt = envValue.tryParseBackend()

    if opt.isSome():
      opt.get()
    else:
      raise newInvalidEnvVarValueError(envVar, envValue)

  envVar.readFromEnv(fetchEnvValue, parseBackendOrRaise)



# Task API

type
  Task {.pure.} = enum
    Test
    Docs
    CleanTest
    CleanDocs
    Clean

  OutputDirBuilder =
    proc (projectDir: AbsoluteDir): Option[AbsoluteDir] {.
      nimcall, noSideEffect
    .}



func taskNames (): array[Task, string] =
  const names = ["test", "docs", "clean_test", "clean_docs", "clean"]

  names


func name (self: Task): string =
  taskNames()[self]


func identifier (self: Task): NimNode =
  self.name().ident()



func testTaskDescription (): string =
  func backendChoice (): string =
    nimBackendShellValues().join($'|')

  [
    "Build the tests and run them.",
    "The backend can be specified with the environment variable",
    fmt""""{EnvVar.NimBackend.shellName()}=({backendChoice()})"."""
  ].join($' ')


func cleanOtherTaskDescription (cleaned: Task): string =
  fmt"""Remove the build directory of the "{cleaned.name()}" task."""


func taskDescriptions (): array[Task, string] =
  const descriptions =
    [
      testTaskDescription(),
      "Build the API doc.",
      Task.Test.cleanOtherTaskDescription(),
      Task.Docs.cleanOtherTaskDescription(),
      "Remove all the build directories."
    ]

  descriptions


func description (self: Task): string =
  taskDescriptions()[self]



func baseOutputDir (self: Task; projectDir: AbsoluteDir): AbsoluteDir =
  projectDir.nimCacheDir() / self.name()



func returnNoOutputDir (_: AbsoluteDir): Option[AbsoluteDir] =
  AbsoluteDir.none()


func taskOutputDirBuilders (): array[Task, OutputDirBuilder] =
  const builders =
    [
      (projectDir: AbsoluteDir) => Task.Test.baseOutputDir(projectDir).some(),
      (projectDir: AbsoluteDir) => Task.Docs.baseOutputDir(projectDir).some(),
      returnNoOutputDir,
      returnNoOutputDir,
      returnNoOutputDir
    ]

  builders


func outputDirBuilder (self: Task): OutputDirBuilder =
  taskOutputDirBuilders()[self]


func outputDir (
  self: Task;
  projectDir: () -> AbsoluteDir
): Option[AbsoluteDir] =
  self
    .outputDirBuilder()
    .option()
    .flatMap((builder: OutputDirBuilder) => projectDir().builder())



# Nimble tasks.

func nimbleProjectName (): string =
  projectName().stripTrailing(Digits).stripTrailing('_')



func libraryModuleDir (): AbsoluteDir =
  projectDir() / nimbleProjectName()


func outputDir (self: Task): Option[AbsoluteDir] =
  self.outputDir(projectDir)



proc getEnvOrEmpty (key: string): string =
  key.getEnv()


proc tryReadEnv (key: string): Option[string] =
  key.some().filter(existsEnv).map(getEnvOrEmpty)


proc tryRmDir (dir: AbsoluteDir) =
  if system.existsDir(dir):
    dir.rmDir()



macro define (self: static Task; body: untyped): untyped =
  let
    selfIdent = self.identifier()
    selfLit = self.newLit()

  quote do:
    task `selfIdent`, `selfLit`.description():
      `body`


macro run (self: static Task) =
  fmt"{self.identifier()}Task".newCall()



define Task.Test:
  func defaultBackend (): Backend =
    Backend.C


  func buildSrcGenDir (module: RelativeFile; backend: Backend): AbsoluteDir =
    Task.Test.outputDir().get() / backend.shellValue() / module


  func binOutDirName (): string =
    "bin"


  func handleJsFlags (backend: Backend): seq[string] =
    if backend == Backend.Js:
      @["-d:nodejs"]
    else:
      @[]


  func buildCompileCmd (module: RelativeFile; backend: Backend): string =
    let
      srcGenDir = module.buildSrcGenDir(backend)
      binOutDir = srcGenDir / binOutDirName()

      jsFlags = backend.handleJsFlags()
      outDirOptions =
        [("nimcache", srcGenDir), ("outdir", binOutDir)].toNimLongOptions()

    @[backend.nimCmdName(), "-r"]
      .concat(jsFlags, outDirOptions, @[module.quoteShell()])
      .join($' ')


  let backend = readNimBackendFromEnv(tryReadEnv).get(defaultBackend())

  withDir libraryModuleDir():
    for module in relativeNimModules($CurDir):
      module.buildCompileCmd(backend).selfExec()



define Task.Docs:
  func buildCompileCmd (module: RelativeFile): string =
    const
      repoUrl = "https://github.com/thenjip/nim-iterator-stream-experiment"
      mainGitBranch = "master"

    let longOptions =
      [
        ("index", "on"),
        ("docRoot", projectDir()),
        ("outdir", Task.Docs.outputDir().get()),
        ("git.url", repoUrl),
        ("git.devel", mainGitBranch),
        ("git.commit", mainGitBranch)
      ].toNimLongOptions()

    @["doc"].concat(longOptions, @[module.quoteShell()]).join($' ')


  for module in relativeNimModules(projectDir()):
    if module.startsWith(nimbleProjectName()):
      module.buildCompileCmd().selfExec()

  withDir Task.Docs.outputDir().get():
    const cssFile = ["nimdoc", "out", "css"].join($ExtSep)

    ["buildIndex", $CurDir].join($' ').selfExec()
    fmt"theindex{ExtSep}html".cpFile(fmt"index{ExtSep}html")
    nimbleProjectName().`/`(cssFile).cpFile(system.getCurrentDir() / cssFile)



define Task.CleanTest:
  Task.Test.outputDir().get().tryRmDir()



define Task.CleanDocs:
  Task.Docs.outputDir().get().tryRmDir()



define Task.Clean:
  Task.CleanTest.run()
  Task.CleanDocs.run()
