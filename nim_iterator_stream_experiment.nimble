import nimble/[envvar, paths, nimcmdline, task, utils]

import std/[macros, options, os, sequtils, strformat, strutils]



func nimbleProjectName (): string =
  projectName().stripTrailing(Digits).stripTrailing('_')



version = "0.1.0"
author = "thenjip"
nimscriptapi.description =
  "An attempt at providing a replacement for closure iterators in Nim with an API similar to Java 8 Stream."
license = "MIT"

requires "nim >= 1.2.0"

installDirs = @[nimbleProjectName()]



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
        ("docRoot", libraryModuleDir()),
        ("outdir", Task.Docs.outputDir().get()),
        ("git.url", repoUrl),
        ("git.devel", mainGitBranch),
        ("git.commit", mainGitBranch)
      ].toNimLongOptions()

    @["doc"].concat(longOptions, @[module.quoteShell()]).join($' ')


  withDir libraryModuleDir():
    for module in relativeNimModules($CurDir):
      module.buildCompileCmd().selfExec()

  withDir Task.Docs.outputDir().get():
    ["buildIndex", $CurDir].join($' ').selfExec()
    fmt"theindex{ExtSep}html".cpFile(fmt"index{ExtSep}html")



define Task.CleanTest:
  Task.Test.outputDir().get().tryRmDir()



define Task.CleanDocs:
  Task.Docs.outputDir().get().tryRmDir()



define Task.Clean:
  Task.CleanTest.run()
  Task.CleanDocs.run()
