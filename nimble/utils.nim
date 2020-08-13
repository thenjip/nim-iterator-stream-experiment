import std/[options, sugar]



func findFirst* [I, T](a: array[I, T]; predicate: T -> bool): Option[I] =
  result = I.none()

  for i, item in a:
    if item.predicate():
      return i.some()
