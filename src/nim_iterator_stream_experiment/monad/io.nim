##[
  The IO monad from Haskell.

  It lets one build a computation that does not need a parameter.
  It can also be used as a wrapper for computations that interact with external
  resources such as memory management, FFI, I/O streams, etc. .
]##



import reader
import ../utils/[chain, unit]

import std/[sugar]



type IO* [T] = () -> T



func toIO* [T](value: T): IO[T] =
  () => value



proc run* [T](self: IO[T]): T =
  self()



func map* [A; B](self: IO[A]; f: A -> B): IO[B] =
  self.chain(f)


func flatMap* [A; B](self: IO[A]; f: A -> IO[B]): IO[B] =
  self.map(f).chain(run)



func bracket* [A; B](before: IO[A]; between: A -> B; after: A -> Unit): IO[B] =
  before.map(between.flatMap((b: B) => A.ask().map(after).map(_ => b)))



when isMainModule:
  import lazymonadlaws
  import ../utils/[lambda, proctypes]

  import std/[os, sequtils, unittest]



  proc run [T](self: IO[T]; _: Unit): T =
    self.run()



  static:
    doAssert(IO[byte] is LazyMonad[byte, Unit])



  suite currentSourcePath().splitFile().name:
    test """"IO[T]" should obey the monad laws.""":
      proc doTest [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC](
        spec: MonadLawsSpec[LA, LMA, LMB, Unit, RT, RM, Unit, AA, AB, AMA, AMB, AMC, Unit]
      ) =
        check:
          spec.checkMonadLaws()


      doTest(
        monadLawsSpec(
          leftIdentitySpec(-5, toIO, i => toIO(-i), unit()),
          rightIdentitySpec(() => newStringOfCap(10), toIO, unit()),
          associativitySpec(
            ('a', true),
            toIO,
            t => (t[0], not t[1]).toIO(),
            (t: (char, bool)) => t[0].`$`().len().toIO(),
            unit()
          )
        )
      )



    test """"IO[T]" without side effects should be compatible with compile time execution.""":
      template doTest [T](
        sut: static[proc (): IO[T] {.nimcall, noSideEffect.}];
        expected: static T
      ): proc () {.nimcall.} =
        (
          proc () =
            const actual = sut().run()

            check:
              actual == expected
        )


      func expected1 (): int =
        1

      func sut1 (): IO[expected1.returnType()] =
        expected1


      func expected2 (): int =
        toSeq(1 .. 10).foldl(a + b, 0)

      func sut2 (): IO[expected2.returnType()] =
        lambda(1 .. 10)
          .map(slice => toSeq(slice.items()))
          .flatMap((s: seq[int]) => s.foldl(a + b, 0).toIO())


      for t in [doTest(sut1, expected1()), doTest(sut2, expected2())]:
        t()



    test "bracket: compute the string's length with final action":
      proc doTest () =
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


      doTest()
