import reader
import ../utils/[ifelse, partialprocs]

import std/[sugar]



type
  Predicate* [T] = Reader[T, bool]



proc test* [T](self: Predicate[T]; value: T): bool =
  self.run(value)



func `not`* [T](self: Predicate[T]): Predicate[T] =
  self.map(partial(not ?_))


func `and`* [T](self, then: Predicate[T]): Predicate[T] =
  (value: T) => self.test(value) and then.test(value)


func `or`* [T](self, `else`: Predicate[T]): Predicate[T] =
  (value: T) => self.test(value) or `else`.test(value)



func ifElse* [A; B](self: Predicate[A]; then, `else`: A -> B): Reader[A, B] =
  (value: A) =>
    self.test(value).ifElse(() => then.run(value), () => `else`.run(value))



func alwaysFalse* [T](_: T): bool =
  false


func alwaysTrue* [T](_: T): bool =
  true



when isMainModule:
  import identity
  import ../utils/[ignore, operators, unit, variables]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"self.test(value)" should return the expected boolean.""":
      proc doTest [T](self: Predicate[T]; value: T; expected: bool) =
        let actual = self.test(value)

        check:
          actual == expected


      doTest(alwaysFalse[ref Defect], nil, false)
      doTest(alwaysTrue[bool], false, true)
      doTest(partial(?:Natural < 10), 4, true)
      doTest((s: string) => s.len() > 3, "a", false)



    test """"not self" should return a predicate that is the negation of "self".""":
      proc doTest [T](self: Predicate[T]; value: T; expected: bool) =
        let
          sut = not self
          actual = sut.test(value)

        check:
          actual == expected


      doTest(alwaysTrue[int], -854, false)
      doTest(partial(22 in ?:set[uint8]), {0u8, 255u8, 6u8}, true)



    test """"self and then" should return a predicate that combines "self" and "then" with a logical "and".""":
      proc doTest [T](self, then: Predicate[T]; value: T; expected: bool) =
        let
          sut = self and then
          actual = sut.test(value)

        check:
          actual == expected


      doTest(alwaysFalse[char], alwaysTrue, '\r', false)
      doTest(partial(?:int > 0), partial(?_ < 100), 91, true)



    test """"self or `else`" should return a predicate that combines "self" and "else" with a logical "or".""":
      proc doTest [T](self, `else`: Predicate[T]; value: T; expected: bool) =
        let
          sut = self or `else`
          actual = sut.test(value)

        check:
          actual == expected


      doTest(alwaysTrue[Positive], i => i > 8, 1, true)
      doTest(partial(0 in ?:seq[int]), partial(1 in ?_), @[2, 5, 1, 7], true)



    test """"self.ifElse(then, `else`)" should return a Reader that will enter only 1 of the 2 paths.""":
      proc doTest [A; B](self: Predicate[A]; then, `else`: A -> B; value: A) =
        const expected = 1.Natural

        var pathTaken = 0.Natural

        proc incrementPathCounter [T](value: T): T =
          pathTaken.modify(plus1).apply(_ => value)

        self
          .ifElse(
            then.map(incrementPathCounter),
            `else`.map(incrementPathCounter)
          ).run(value)
          .ignore()

        check:
          pathTaken.read() == expected


      doTest(alwaysTrue[Unit], itself[Unit], itself, unit())
      doTest(alwaysFalse[uint16], partial($ ?:uint16), _ => "abc", 9484)
