import identity
import ../utils/[chain, unit]

import std/[sugar]



##[
  The IO monad from Haskell.
]##



type IO* [T] = () -> T



func toIO* [T](value: T): IO[T] =
  () => value



proc run* [T](self: IO[T]): T =
  self()



func flatMap* [A; B](self: IO[A]; f: A -> IO[B]): IO[B] =
  self.chain(f).chain(run)


func map* [A; B](self: IO[A]; f: A -> B): IO[B] =
  self.flatMap((a: A) => (() => a.f()))



func bracket* [A; B](
  before: IO[A];
  between: A -> B;
  after: A -> Unit
): IO[B] =
  before.map(a => a.between().apply(b => a.after().apply(_ => b)))



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "bracket: compute the string's length with final action":
      proc bracketTest () =
        var afterExecuted = false
        let
          initial = "abc"
          betweenFunc = (s: string) => s.len()
          expected = initial.betweenFunc()
          sut =
            initial
            .toIO()
            .bracket(
              betweenFunc,
              proc (_: auto): Unit =
                afterExecuted = true
            ).run()

        check:
          sut == expected
          afterExecuted


      bracketTest()
