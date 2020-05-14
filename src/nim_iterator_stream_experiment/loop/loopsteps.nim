import ../monad/[optional]
import ../optics/[lens]

import std/[sugar]



type
  ZeroStep* = object
    discard

  SingleStep* = object
    consumed: bool

  TakeWhileStep* [S; T] = object
    step: S
    item: Optional[T]



func zeroStep* (): ZeroStep =
  ZeroStep()



func singleStep* (consumed: bool): SingleStep =
  SingleStep(consumed: consumed)


func isConsumed* (X: typedesc[SingleStep]): Lens[X, bool] =
  lens(
    (self: X) => self.consumed,
    (_: X, consumed: bool) => singleStep(consumed)
  )



func takeWhileStep* [S; T](step: S; item: Optional[T]): TakeWhileStep[S, T] =
  TakeWhileStep[S, T](step: step, item: item)


func step* [S; T](X: typedesc[TakeWhileStep[S, T]]): Lens[X, S] =
  lens(
    (self: X) => self.step,
    (self: X, step: S) => takeWhileStep(step, self.item)
  )


func item* [S; T](X: typedesc[TakeWhileStep[S, T]]): Lens[X, Optional[T]] =
  lens(
    (self: X) => self.item,
    (self: X, item: Optional[T]) => takeWhileStep(self.step, item)
  )



when isMainModule:
  import ../monad/[reader]
  import ../optics/[lenslaws]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"SingleStep.isConsumed()" should return a lens that obeys the lens laws.""":
      proc doTest (spec: LensLawsSpec[SingleStep, bool]) =
        check:
          SingleStep.isConsumed().checkLensLaws(spec)


      doTest(
        lensLawsSpec(
          identitySpec(singleStep(false)),
          retentionSpec(singleStep(true), false),
          doubleWriteSpec(singleStep(false), true, false)
        )
      )



    test """"TakeWhileStep[S, T].step()" should return a lens that obeys the lens laws.""":
      proc doTest [S; T](spec: LensLawsSpec[TakeWhileStep[S, T], S]) =
        check:
          TakeWhileStep[S, T].step().checkLensLaws(spec)


      doTest(
        lensLawsSpec(
          identitySpec(takeWhileStep(1.Natural, "\0".toSome())),
          retentionSpec(takeWhileStep(0.Natural, "".toSome()), 19.Natural),
          doubleWriteSpec(
            takeWhileStep(Natural.high(), "abc".toSome()),
            Natural.high().pred(),
            8415
          )
        )
      )



    test """"TakeWhileStep[S, T].item()" should return a lens that obeys the lens laws.""":
      proc doTest [S; T](spec: LensLawsSpec[TakeWhileStep[S, T], Optional[T]]) =
        check:
          TakeWhileStep[S, T].item().checkLensLaws(spec)


      doTest(
        lensLawsSpec(
          identitySpec(takeWhileStep({0i16, 7i16}, @[2.1].toSome())),
          retentionSpec(
            takeWhileStep(set[int16].default(), @[-Inf, NaN].toSome()),
            @[Inf].toSome()
          ),
          doubleWriteSpec(
            takeWhileStep({-16000i16}, @[-1.0, 9.5, 4.894].toSome()),
            seq[float].default().toSome(),
            @[-54965.5, -1896.018].toSome()
          )
        )
      )
