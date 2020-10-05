import loop, loop/[loopscope]
import ../monad/[optional, reader, predicate]
import ../optics/[lens]
import ../utils/[pair, partialprocs, somenatural]

import std/[sugar]



type
  EmptyStep* = object
    discard

  SingleStep* = object
    consumed: bool

  ZipStep* [SA; SB] = object
    ## Since 0.4.0.
    left: SA
    right: SB

  LimitStep* [S; N: SomeNatural] = object
    step: S
    count: N

  SkipStep* [S; N: SomeNatural] = object
    step: S
    count: N

  TakeWhileStep* [S; T] = object
    step: S
    item: Optional[T]



func emptyStep* (): EmptyStep =
  EmptyStep()



func singleStep* (consumed: bool): SingleStep =
  SingleStep(consumed: consumed)


func isConsumed* (X: typedesc[SingleStep]): Lens[X, bool] =
  lens(
    (self: X) => self.consumed,
    (_: X, consumed: bool) => singleStep(consumed)
  )



func zipStep* [SA; SB](left: SA; right: SB): ZipStep[SA, SB] =
  ## Since 0.4.0.
  ZipStep[SA, SB](left: left, right: right)


func left* [SA; SB](X: typedesc[ZipStep[SA, SB]]): Lens[X, SA] =
  ## Since 0.4.0.
  lens((self: X) => self.left, (self: X, left: SA) => zipStep(left, self.right))


func right* [SA; SB](X: typedesc[ZipStep[SA, SB]]): Lens[X, SB] =
  ## Since 0.4.0.
  lens(
    (self: X) => self.right,
    (self: X, right: SB) => zipStep(self.left, right)
  )


func toPair* [SA; SB](self: ZipStep[SA, SB]): Pair[SA, SB] =
  ## Since 0.4.0.
  (self.left, self.right)


func toZipStep* [SA; SB](pair: Pair[SA, SB]): ZipStep[SA, SB] =
  ## Since 0.4.0.
  zipStep(pair.first, pair.second)


proc hasMore* [SA; SB](
  self: ZipStep[SA, SB];
  leftCond: Condition[SA];
  rightCond: Condition[SB]
): bool =
  ## Since 0.4.0.
  self.toPair().apply(leftCond, rightCond).reduce(partial(?_ and ?_))


proc generate* [SA; A; SB; B](
  self: ZipStep[SA, SB];
  leftGen: Generator[SA, A];
  rightGen: Generator[SB, B]
): Pair[A, B] =
  ## Since 0.4.0.
  self.toPair().apply(leftGen, rightGen)


proc next* [SA; SB](
  self: ZipStep[SA, SB];
  leftStepper: Stepper[SA];
  rightStepper: Stepper[SB]
): ZipStep[SA, SB] =
  ## Since 0.4.0.
  self.toPair().apply(leftStepper, rightStepper).toZipStep()



func limitStep* [S; N: SomeNatural](step: S; count: N): LimitStep[S, N] =
  LimitStep[S, N](step: step, count: count)


func step* [S; N: SomeNatural](X: typedesc[LimitStep[S, N]]): Lens[X, S] =
  lens(
    (self: X) => self.step,
    (self: X, step: S) => limitStep(step, self.count)
  )


func count* [S; N: SomeNatural](X: typedesc[LimitStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => limitStep(self.step, count)
  )



func skipStep* [S; N: SomeNatural](step: S; count: N): SkipStep[S, N] =
  SkipStep[S, N](step: step, count: count)


func step* [S; N: SomeNatural](X: typedesc[SkipStep[S, N]]): Lens[X, S] =
  lens((self: X) => self.step, (self: X, step: S) => skipStep(step, self.count))


func count* [S; N: SomeNatural](X: typedesc[SkipStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => skipStep(self.step, count)
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
  import ../optics/[lenslaws]
  import ../utils/[convert]

  import std/[os, unittest]


  proc main () =
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



      test """"ZipStep[SA, SB].left()" should return a lens that verifies the lens laws.""":
        proc doTest [SA; SB](spec: LensLawsSpec[ZipStep[SA, SB], SA]) =
          check:
            ZipStep[SA, SB].left().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(zipStep(0, 'a')),
            retentionSpec(zipStep(1, 'b'), -50),
            doubleWriteSpec(zipStep(10, '\0'), int.low(), int.high())
          )
        )



      test """"ZipStep[SA, SB].right()" should return a lens that verifies the lens laws.""":
        proc doTest [SA; SB](spec: LensLawsSpec[ZipStep[SA, SB], SB]) =
          check:
            ZipStep[SA, SB].right().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(zipStep(24, 'z')),
            retentionSpec(zipStep(987, '\''), '\n'),
            doubleWriteSpec(zipStep(10, '\0'), '-', '\a')
          )
        )



      test """"LimitStep[S, N].step()" should return a lens that obeys the lens laws.""":
        proc doTest [S; N](spec: LensLawsSpec[LimitStep[S, N], S]) =
          check:
            LimitStep[S, N].step().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(limitStep(0, 50 as Natural)),
            retentionSpec(limitStep(int.high(), 6 as Natural), 1),
            doubleWriteSpec(
              limitStep(81435, 463 as Natural),
              int.high() div 2,
              1563
            )
          )
        )



      test """"LimitStep[S, N].count()" should return a lens that obeys the lens laws.""":
        proc doTest [S; N](spec: LensLawsSpec[LimitStep[S, N], N]) =
          check:
            LimitStep[S, N].count().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(limitStep("a", 12u32)),
            retentionSpec(limitStep("", 17u32), uint32.low()),
            doubleWriteSpec(limitStep("abc", 9u32), uint32.high(), 6753)
          )
        )



      test """"SkipStep[S, N].step()" should return a lens that obeys the lens laws.""":
        proc doTest [S; N](spec: LensLawsSpec[SkipStep[S, N], S]) =
          check:
            SkipStep[S, N].step().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(skipStep(0, 50u)),
            retentionSpec(skipStep(int.high(), 6u), 1),
            doubleWriteSpec(skipStep(81435, 463u), int.high() div 2, 1563)
          )
        )



      test """"SkipStep[S, N].count()" should return a lens that obeys the lens laws.""":
        proc doTest [S; N](spec: LensLawsSpec[SkipStep[S, N], N]) =
          check:
            SkipStep[S, N].count().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(skipStep("a", 12u32)),
            retentionSpec(skipStep("", 17u32), uint32.low()),
            doubleWriteSpec(skipStep("abc", 9u32), uint32.high(), 6753)
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
        proc doTest [S; T](
          spec: LensLawsSpec[TakeWhileStep[S, T], Optional[T]]
        ) =
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



  main()
