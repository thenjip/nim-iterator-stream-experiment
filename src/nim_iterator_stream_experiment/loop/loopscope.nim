import ../monad/[predicate, reader]
import ../optics/[focus, lens]
import ../utils/[convert, ignore, unit, variables]

import std/[sugar]



type
  Condition* [T] = Predicate[T]
  Stepper* [S] = Reader[S, S]

  LoopScope* [S] = object
    condition: Condition[S]
    stepper: Stepper[S]



func condition* [T](predicate: Predicate[T]): Condition[T] =
  predicate


func stepper* [S](reader: Reader[S, S]): Stepper[S] =
  reader



func looped* [S](condition: Condition[S]; stepper: Stepper[S]): LoopScope[S] =
  LoopScope[S](condition: condition, stepper: stepper)


func looped* [S](condition: Condition[S]; stepper: S -> S): LoopScope[S] =
  condition.looped(stepper as Stepper[S])



func condition* [S](X: typedesc[LoopScope[S]]): Lens[X, Condition[S]] =
  lens(
    (self: X) => self.condition,
    (self: X, condition: Condition[S]) => condition.looped(self.stepper)
  )


func stepper* [S](X: typedesc[LoopScope[S]]): Lens[X, Stepper[S]] =
  lens(
    (self: X) => self.stepper,
    (self: X, stepper: Stepper[S]) => self.condition.looped(stepper)
  )



proc run* [S](self: LoopScope[S]; initial: () -> S; body: S -> Unit): S =
  result = initial()

  while self.condition.test(result):
    body.run(result).ignore()
    result.modify(self.stepper).ignore()


proc run* [S](self: LoopScope[S]; initial: () -> S): S =
  self.run(initial, doNothing)



func asReader* [S](self: LoopScope[S]; body: S -> Unit): Reader[S, S] =
  (initial: S) => self.run(() => initial, body)


func asReader* [S](self: LoopScope[S]): Reader[S, S] =
  self.asReader(doNothing)



proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  conditionMapper: Condition[SA] -> Condition[SB];
  stepperMapper: Stepper[SA] -> Stepper[SB]
): LoopScope[SB] =
  proc buildResult [X: self.typeof()](
    self: X;
    conditionMapper: conditionMapper.typeof();
    stepperMapper: stepperMapper.typeof()
  ): result.typeof() =
    self
      .read(X.condition())
      .conditionMapper()
      .looped(self.read(X.stepper()).stepperMapper())

  self.buildResult(conditionMapper, stepperMapper)


proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB]
): LoopScope[SB] =
  self.mapSteps(
    (condition: Condition[SA]) => extractor.map(condition),
    stepperMapper
  )


proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  extractor: SB -> SA;
  wrapper: SA -> SB
): LoopScope[SB] =
  self.mapSteps(extractor, stepper => extractor.map(stepper).map(wrapper))



func breakIf* [S](self: LoopScope[S]; isBroken: Predicate[S]): LoopScope[S] =
  self.modify(self.typeof().condition(), hasMore => hasMore and not isBroken)



when isMainModule:
  import ../monad/[identity]
  import ../optics/[lenslaws]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"LoopScope[S].condition()" should return a lens that obeys the lens laws.""":
      proc doTest [S](
        spec: LensLawsSpec[LoopScope[S], Condition[S]]
      ) =
        check:
          LoopScope[S].condition().checkLensLaws(spec)


      proc runTest1 () =
        let loopScope = looped((i: int32) => i < 53, i => i + 1)

        proc doRun (S: typedesc[loopScope.typeof().S]) =
          doTest(
            lensLawsSpec(
              identityLawSpec(loopScope),
              retentionLawSpec(loopScope, condition((i: S) => i < 100)),
              doubleWriteLawSpec(
                loopScope,
                condition((i: S) => i > 5),
                (i: S) => i != 7
              )
            )
          )

        doRun(loopScope.typeof().S)


      runTest1()



    test """"LoopScope[S].stepper()" should return a lens that obeys the lens laws.""":
      proc doTest [S](spec: LensLawsSpec[LoopScope[S], Stepper[S]]) =
        check:
          LoopScope[S].stepper().checkLensLaws(spec)


      proc runTest1 () =
        let loopScope = looped((c: char) => c in {'a'..'z'}, c => c.succ())

        proc doRun (S: typedesc[loopScope.typeof().S]) =
          doTest(
            lensLawsSpec(
              identityLawSpec(loopScope),
              retentionLawSpec(loopScope, stepper((c: S) => c.pred())),
              doubleWriteLawSpec(loopScope, stepper((c: S) => '0'), itself[S])
            )
          )

        doRun(loopScope.typeof().S)


      runTest1()



    test """Using "sut.run(initial, body)" to count up to "expected" starting at 0 should return "expected".""":
      proc doTest [N: SomeUnsignedInt](expected: N) =
        let sut = looped((i: N) => i < expected, i => i + 1.N)

        check:
          sut.run(() => 0.N, doNothing) == expected


      doTest(100)
      doTest(56u8)
      doTest(4u32)
      doTest(0)
      doTest(1u64)



    test """Using "sut.run(initial, body)" to copy an array into another should return an array equal to the original.""":
      proc doTest [I: Ordinal, T](expected: array[I, T]) =
        var copy: expected.typeof()

        let sut = looped((i: I) => i < expected.len(), i => i.succ())

        sut
          .run(() => expected.low(), proc (i: I): Unit = copy[i] = expected[i])
          .ignore()

        check:
          copy == expected


      doTest(array[0, char].default())
      doTest(["a", "0123", "abc"])