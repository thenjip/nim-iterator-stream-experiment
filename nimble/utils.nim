import paths

import std/[options, os, strformat, strutils, sugar]



func findFirst* [I, T](a: array[I, T]; predicate: T -> bool): Option[I] =
  result = I.none()

  for i, item in a:
    if item.predicate():
      return i.some()



func stripTrailing* (input: string; chars: set[char]): string =
  result = input

  result.removeSuffix(chars)


func stripTrailing* (input: string; c: char): string =
  input.stripTrailing({c})



func joinWithSpace* (a: openArray[string]): string =
  a.join($' ')



iterator relativeNimModules* (dir: AbsoluteDir): RelativeFile =
  for file in dir.walkDirRec(relative = true):
    if file.endsWith(fmt"{ExtSep}nim"):
      yield file
