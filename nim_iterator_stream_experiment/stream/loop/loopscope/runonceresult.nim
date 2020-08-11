import ../../../optics/[lens]
import ../../../monad/[optional]

import std/[sugar]



type
  RunOnceResult* [S; T] = object
    step: S
    item: Optional[T]



func runOnceResult* [S; T](step: S; item: Optional[T]): RunOnceResult[S, T] =
  RunOnceResult[S, T](step: step, item: item)



func step* [SA; T](
  X: typedesc[RunOnceResult[SA, T]]; SB: typedesc
): PLens[X, SA, SB, RunOnceResult[SB, T]] =
  lens(
    (self: X) => self.step,
    (self: X, step: SB) => runOnceResult(step, self.item)
  )


func step* [S; T](X: typedesc[RunOnceResult[S, T]]): Lens[X, S] =
  X.step(S)


func item* [S; A](
  X: typedesc[RunOnceResult[S, A]]; B: typedesc
): PLens[X, Optional[A], Optional[B], RunOnceResult[S, B]] =
  lens(
    (self: X) => self.item,
    (self: X, item: Optional[B]) => runOnceResult(self.step, item)
  )


func item* [S; T](X: typedesc[RunOnceResult[S, T]]): Lens[X, Optional[T]] =
  X.item(T)



when isMainModule:
  import ../../../optics/[lenslaws]

  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"RunOnceResult[S, T].step()" should return a lens that verifies the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[RunOnceResult[S, T], S]) =
          check:
            RunOnceResult[S, T].step().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(runOnceResult(1, char.toNone())),
            retentionSpec(runOnceResult(-1, 'a'.toSome()), 0),
            doubleWriteSpec(
              runOnceResult(int.low(), char.high().toSome()),
              int.high(),
              -942
            )
          )
        )



      test """"RunOnceResult[S, T].item()" should return a lens that verifies the lens laws.""":
        proc doTest [S; T](
          spec: LensLawsSpec[RunOnceResult[S, T], Optional[T]]
        ) =
          check:
            RunOnceResult[S, T].item().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(runOnceResult(1, char.toNone())),
            retentionSpec(runOnceResult(-1, 'a'.toSome()), char.toNone()),
            doubleWriteSpec(
              runOnceResult(int.low(), char.high().toSome()),
              char.low().toSome(),
              '\a'.toSome()
            )
          )
        )



  main()
