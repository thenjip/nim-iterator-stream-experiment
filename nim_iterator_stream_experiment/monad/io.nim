##[
  The IO monad from Haskell.

  It lets one build a computation that has no parameters.
  It can also be used as a wrapper for computations that interact with external
  resources such as memory management, FFI, I/O, etc. .

  Examples can be found `here <https://github.com/thenjip/nim-iterator-stream-experiment/tree/d55b81bcdc94d01372305befb9accbf9fdad32aa/examples/monad/io>`_.
]##



import reader
import ../utils/[chain, ignore, unit]

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
    If an exception is raised anywhere in `before` or `between`, `after` will
    not be executed.
  ]##
  before.map(between.flatMap((b: B) => after.chain(_ => b)))


func tryBracket* [A; B](
  before: IO[A];
  `try`: A -> B;
  `finally`: A -> Unit
): IO[B] =
  ##[
    Any exception raised in `try` will be reraised in the returned `IO`.
    Whether an exception was raised, `finally` will be executed once.
  ]##
  (
    proc (): B =
      let a = before.run()

      try:
        a.`try`()
      except:
        raise getCurrentException()
      finally:
        a.`finally`().ignore()
  )



when isMainModule:
  import identity, lazymonadlaws
  import ../utils/[call, lambda, partialprocs, proctypes, variables]

  import std/[os, sequtils, unittest]



  proc run [T](self: IO[T]; _: Unit): T =
    self.run()



  static:
    doAssert(IO[byte] is LazyMonad[byte, Unit])



  proc main () =
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



      test """"tryBracket" should run the "before", "try" and "finally" procs once and in this order when no exception is raised.""":
        type ExecBlock {.pure.} = enum
          Before
          Try
          Finally


        proc addAndReturn [T](
          order: var seq[ExecBlock];
          added: ExecBlock;
          returned: T
        ): T =
          order.add(added)

          returned


        proc runTryBracket [A; B](
          before: IO[A];
          `try`: A -> B;
          `finally`: A -> Unit
        ): tuple[`result`: B; execOrder: seq[ExecBlock]] =
          var execOrder: result.execOrder.typeof() = @[]

          let execResult =
            before
              .map(partial(execOrder.addAndReturn(ExecBlock.Before, ?_)))
              .tryBracket(
                `try`.map(partial(execOrder.addAndReturn(ExecBlock.Try, ?_))),
                `finally`.map(
                  partial(execOrder.addAndReturn(ExecBlock.Finally, ?_))
                )
              ).run()

          (execResult, execOrder)


        proc doTest [A; B](
          before: IO[A];
          `try`: A -> B;
          `finally`: A -> Unit;
          expectedResult: B
        ) =
          let
            expected = (expectedResult, toSeq(ExecBlock.items()))
            actual = before.runTryBracket(`try`, `finally`)

          check:
            actual == expected


        proc runTest1 () =
          let start = "abc"

          doTest(() => start, s => s.len(), _ => unit(), start.len())


        runTest1()



      test """"tryBracket" should re-raise the exception when an exception has been raised in the "try" proc and after running the "finally" proc.""":
        proc doTest [A; B](before: IO[A]; `try`: A -> B; `finally`: A -> Unit) =
          var finallyExecuted = false

          let tryBlock =
            before.tryBracket(
              `try`.map(proc (_: B): B = raise ValueError.newException("")),
              `finally`.map(proc (_: auto): Unit = finallyExecuted = true)
            )

          expect Exception:
            tryBlock.run().ignore()

          check:
            finallyExecuted


        doTest(() => 12, itself, _ => unit())



  main()
