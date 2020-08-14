import std/[options, strutils, sugar]



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
