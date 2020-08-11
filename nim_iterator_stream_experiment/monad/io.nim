##[
  The IO monad from Haskell.

  It lets one build a computation that has no parameters.
  It can also be used as a wrapper for computations that interact with external
  resources such as memory management, FFI, I/O, etc. .

  Examples can be found `here <https://github.com/thenjip/nim-iterator-stream-experiment/tree/d55b81bcdc94d01372305befb9accbf9fdad32aa/examples/monad/io>`_.
]##



import reader
import ../utils/[chain, lambda, ignore, unit]

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
  ##[
    An adaptation of stack management based on scopes for other resources like
    files, manually allocated memory blocks, ... .

    Here, the scope starts with `before` and ends with `after`.

    If an exception is raised anywhere in `before` or `between`, `after` will
    not be executed.
  ]##
  before.map(between.flatMap((b: B) => after.chain(_ => b)))


func tryBracket* [T](`try`: IO[T]; `finally`: IO[Unit]): IO[T] =
  lambda:
    try:
      `try`.run()
    except:
      raise getCurrentException()
    finally:
      `finally`.run().ignore()



when isMainModule:
  import lazymonadlaws
  import ../utils/[call, proctypes, variables]

  import std/[os, sequtils, unittest]



  proc run [T](self: IO[T]; _: Unit): T =
    self.run()



  static:
    doAssert(IO[byte] is LazyMonad[byte, Unit])



  proc main () =
    suite currentSourcePath().splitFile().name:
      type
        ExecBlock {.pure.} = enum
          Try
          Finally



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
          sut: IO[T]{noSideEffect};
          expected: static T
        ): proc () =
          (
            proc () =
              const actual = sut.run()

              check:
                actual == expected
          )


        func expected1 (): int =
          1


        func expected2 (): int =
          toSeq(1 .. 10).foldl(a + b, 0)

        func sut2 (): IO[expected2.returnType()] =
          lambda(1 .. 10)
            .map(slice => toSeq(slice.items()))
            .flatMap((s: seq[int]) => s.foldl(a + b, 0).toIO())


        for t in [
          doTest(expected1, expected1.call()),
          doTest(sut2.call(), expected2.call())
        ]:
          t.call()



      test """"after" in "self.bracket(between, after)" should be executed after "between".""":
        proc doTest () =
          var address = new byte

          let
            expected = address.read()
            actual =
              unit()
                .lambda()
                .bracket(
                  _ => address.read(),
                  _ => address.write(nil).doNothing()
                ).run()

          check:
            actual == expected



        doTest()



      test """"tryBracket" should run the "try" and "finally" procs once and in this order when no exception is raised.""":
        proc runTryBracket [T](
          `try`: IO[T];
          `finally`: IO[Unit]
        ): tuple[`result`: T; execOrder: seq[ExecBlock]] =
          var execOrder: result.execOrder.typeof() = @[]

          let execResult =
            `try`
              .map(
                proc (`out`: T): T =
                  execOrder.add(ExecBlock.Try)

                  out
              ).tryBracket(
                `finally`
                  .map(proc (_: auto): Unit = execOrder.add(ExecBlock.Finally))
              ).run()

          (execResult, execOrder)



        proc doTest [T](`try`: IO[T]; `finally`: IO[Unit]; expectedResult: T) =
          let
            expected = (expectedResult, @[ExecBlock.Try, ExecBlock.Finally])
            actual = `try`.runTryBracket(`finally`)

          check:
            actual == expected



        doTest(() => "abc", () => unit(), "abc")



      test """"tryBracket" should re-raise the exception when an exception has been raised in the "try" proc and after running the "finally" proc.""":
        proc doTest [T](`try`: IO[T]; `finally`: IO[Unit]) =
          var finallyExecuted = false

          let tryBlock =
            `try`
              .map(proc (_: auto): Unit = raise ValueError.newException(""))
              .tryBracket(
                `finally`.map(proc (_: auto): Unit = finallyExecuted = true)
              )

          expect Exception:
            tryBlock.run().ignore()

          check:
            finallyExecuted


        doTest(() => 12, () => unit())



  main()
