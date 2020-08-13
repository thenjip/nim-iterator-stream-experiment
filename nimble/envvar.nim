import utils

import std/[options, os, strformat, sugar]



type
  EnvVar* {.pure.} = enum
    NimBackend

  Backend* {.pure.} = enum
    C,
    Cxx,
    Js

  InvalidEnvVarValueError* = object of CatchableError



func shellNames* (): array[EnvVar, string] =
  const names = ["NIM_BACKEND"]

  names


func shellName* (self: EnvVar): string =
  shellNames()[self]



func nimBackendShellValues* (): array[Backend, string] =
  const values = ["c", "cxx", "js"]

  values


func shellValue* (self: Backend): string =
  nimBackendShellValues()[self]



func nimCmdNames* (): array[Backend, string] =
  const names = ["cc", "cpp", "js"]

  names


func nimCmdName* (self: Backend): string =
  nimCmdNames()[self]



func newInvalidEnvVarValueError* (
  name: string;
  value: string
): ref InvalidEnvVarValueError =
  InvalidEnvVarValueError.newException(&"{name}=\"{value}\"")


func newInvalidEnvVarValueError* (
  self: EnvVar;
  value: string
): ref InvalidEnvVarValueError =
  newInvalidEnvVarValueError(self.shellName(), value)



proc readFromEnv* [T](self: EnvVar; parseValue: string -> T): Option[T] =
  self.shellName().some().filter(existsEnv).map(ev => ev.getEnv().parseValue())



func tryParseBackend* (envValue: string): Option[Backend] =
  nimBackendShellValues().findFirst(value => value == envValue)


proc readNimBackendFromEnv* (): Option[Backend] =
  const envVar = EnvVar.NimBackend

  func parseBackendOrRaise (envValue: string): Backend =
    let opt = envValue.tryParseBackend()

    if opt.isSome():
      opt.get()
    else:
      raise newInvalidEnvVarValueError(envVar, envValue)

  envVar.readFromEnv(parseBackendOrRaise)
