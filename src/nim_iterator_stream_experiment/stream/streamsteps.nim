import ../optics/[lens]

import std/[sugar]



type
  LimitStep* [S; N: SomeUnsignedInt] = object
    step: S
    count: N

  SkipStep* [S; N: SomeUnsignedInt] = object
    step: S
    count: N



func limitStep* [S; N](step: S; count: N): LimitStep[S, N] =
  LimitStep[S, N](step: step, count: count)


func step* [S; N](X: typedesc[LimitStep[S, N]]): Lens[X, S] =
  lens(
    (self: X) => self.step,
    (self: X, step: S) => limitStep(step, self.count)
  )


func count* [S; N](X: typedesc[LimitStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => limitStep(self.step, count)
  )



func skipStep* [S; N](step: S; count: N): SkipStep[S, N] =
  SkipStep[S, N](step: step, count: count)


func step* [S; N](X: typedesc[SkipStep[S, N]]): Lens[X, S] =
  lens((self: X) => self.step, (self: X, step: S) => skipStep(step, self.count))


func count* [S; N](X: typedesc[SkipStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => skipStep(self.step, count)
  )



when isMainModule:
  import ../monad/[reader]
  import ../optics/[lenslaws]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"LimitStep[S, N].step()" should return a lens that obeys the lens laws.""":
      proc doTest [S; N](spec: LensLawsSpec[LimitStep[S, N], S]) =
        check:
          LimitStep[S, N].step().checkLensLaws(spec)


      doTest(
        lensLawsSpec(
          identitySpec(limitStep(0, 50u)),
          retentionSpec(limitStep(int.high(), 6u), 1),
          doubleWriteSpec(limitStep(81435, 463u), int.high() div 2, 1563)
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
