import std/[os]



type
  AbsoluteDir* = string
  AbsoluteFile* = string

  RelativeDir* = string
  RelativeFile* = string



func nimcacheDirName (): string =
  ".nimcache"


func nimcacheDir* (projectDir: AbsoluteDir): AbsoluteDir =
  projectDir / nimcacheDirName()
