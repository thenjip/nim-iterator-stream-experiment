import reader

import std/[sugar]



type
  Predicate* [T] = Reader[T, bool]



func test* [T](self: Predicate[T]; value: T): bool =
  self.run(value)



func `not`* [T](self: Predicate[T]): Predicate[T] =
  self.map(test => not test)


func `and`* [T](self, then: Predicate[T]): Predicate[T] =
  (value: T) => self.test(value) and then.test(value)


func `or`* [T](self, `else`: Predicate[T]): Predicate[T] =
  (value: T) => self.test(value) or `else`.test(value)



func alwaysFalse* [T](_: T): bool =
  false


func alwaysTrue* [T](_: T): bool =
  true



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"self.test(value)" should return the expected boolean.""":
      proc doTest [T](self: Predicate[T]; tested: T; expected: bool) =
        check:
          self.test(tested) == expected


      doTest(alwaysFalse[ref Defect], nil, false)
      doTest(alwaysTrue[bool], false, true)
      doTest((i: Natural) => i < 10, 4, true)
      doTest((s: string) => s.len() > 3, "a", false)



    test """"not self" should return a predicate that is the negation of "self".""":
      proc doTest [T](self: Predicate[T]; tested: T; expected: bool) =
        let sut = not self

        check:
          sut.test(tested) == expected


      doTest(alwaysTrue[int], -854, false)
      doTest((s: set[uint8]) => s.contains(22), {0u8, 255u8, 6u8}, true)



    test """"self and then" should return a predicate that combines "self" and "then" with a logical "and".""":
      proc doTest [T](self, then: Predicate[T]; tested: T; expected: bool) =
        let sut = self and then

        check:
          sut.test(tested) == expected


      doTest(alwaysFalse[char], alwaysTrue, '\r', false)
      doTest((i: int) => i > 0, i => i < 100, 91, true)



    test """"self or else" should return a predicate that combines "self" and "else" with a logical "or".""":
      proc doTest [T](self, `else`: Predicate[T]; tested: T; expected: bool) =
        let sut = self or `else`

        check:
          sut.test(tested) == expected


      doTest(alwaysTrue[Positive], i => i > 8, 1, true)
      doTest(
        (s: seq[int]) => s.contains(0),
        s => s.contains(1),
        @[2, 5, 1, 7],
        true
      )
