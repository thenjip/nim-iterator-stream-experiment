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



func map* [A; B](self: IO[A]; f: A -> B): IO[B] =
  self.chain(f)


func flatMap* [A; B](self: IO[A]; f: A -> IO[B]): IO[B] =
  self.map(f).chain(run)



func bracket* [A; B](
  before: IO[A];
  between: A -> B;
  after: A -> Unit
): IO[B] =
  before.map(a => a.between().apply(b => a.after().apply(_ => b)))



when isMainModule:
  import lazymonadlaws

  import std/[os, unittest]



  func lift [T](value: T): IO[T] =
    value.toIO()


  proc run [T](self: IO[T]; _: Unit): T =
    self.run()



  static:
    doAssert(IO[char] is LazyMonad[char, Unit])



  suite currentSourcePath().splitFile().name:
    test """"IO[T]" should obey the monad laws.""":
      proc doTest [LT; LM; RT; AA; AB; AMA; AMB](
        spec: MonadLawsSpec[LT, LM, Unit, RT, Unit, AA, AB, AMA, AMB, Unit]
      ) =
        check:
          spec.checkMonadLaws()

      doTest(
        monadLawsSpec(
          leftIdentitySpec(-5, i => toIO(-i), unit()),
          rightIdentitySpec(() => newStringOfCap(10), unit()),
          associativitySpec(
            ('a', true),
            t => (t[0], not t[1]).toIO(),
            (t: (char, bool)) => t[0].`$`().len().toIO(),
            unit()
          )
        )
      )



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
