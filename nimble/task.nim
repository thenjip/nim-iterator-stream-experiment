import envvar, paths

import std/[macros, os, options, strformat, strutils, sugar]



type
  Task* {.pure.} = enum
    Test
    Docs
    CleanTest
    CleanDocs
    Clean

  OutputDirBuilder* =
    proc (projectDir: AbsoluteDir): AbsoluteDir {.nimcall, noSideEffect.}


func taskNames* (): array[Task, string] =
  const names = ["test", "docs", "clean_test", "clean_docs", "clean"]

  names


func name* (self: Task): string =
  taskNames()[self]


func identifier* (self: Task): NimNode =
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


func taskDescriptions* (): array[Task, string] =
  const descriptions =
    [
      testTaskDescription(),
      "Build the API doc.",
      Task.Test.cleanOtherTaskDescription(),
      Task.Docs.cleanOtherTaskDescription(),
      "Remove all the build directories."
    ]

  descriptions


func description* (self: Task): string =
  taskDescriptions()[self]



func baseOutputDir (self: Task; projectDir: AbsoluteDir): AbsoluteDir =
  projectDir.nimCacheDir() / self.name()


func taskOutputDirBuilders* (): array[Task, OutputDirBuilder] =
  const builders =
    [
      (projectDir: AbsoluteDir) => Task.Test.baseOutputDir(projectDir),
      (projectDir: AbsoluteDir) => Task.Docs.baseOutputDir(projectDir),
      nil,
      nil,
      nil
    ]

  builders


func outputDirBuilder* (self: Task): OutputDirBuilder =
  taskOutputDirBuilders()[self]


func outputDir* (
  self: Task;
  projectDir: () -> AbsoluteDir
): Option[AbsoluteDir] =
  self.outputDirBuilder().option().map(builder => projectDir().builder())
