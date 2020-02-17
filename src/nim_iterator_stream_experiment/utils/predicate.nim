import std/[sugar]



type
  Predicate* [T] = T -> bool



func test* [T](self: Predicate[T]; value: T): bool =
  value.self()



func `not`* [T](self: Predicate[T]): Predicate[T] =
  (item: T) => not item.self()


func `and`* [T](self, then: Predicate[T]): Predicate[T] =
  (item: T) => item.self() and item.then()


func `or`* [T](self, `else`: Predicate[T]): Predicate[T] =
  (item: T) => item.self() or item.`else`()



func alwaysFalse* [T](_: T): bool =
  false


func alwaysTrue* [T](_: T): bool =
  true



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      discard
