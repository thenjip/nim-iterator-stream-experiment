##[
  A structure that encloses a looped computation between a condition and a
  stepper.
]##



import ../monad/[identity, optional, predicate, reader]
import ../optics/[focus, plens, lens]
import ../utils/[convert, ifelse, ignore, partialprocs, unit, variables]

import std/[sugar]



type
  Condition* [T] = Predicate[T]
  Stepper* [S] = Reader[S, S]

  RunOnceResult* [S; T] = object
    step: S
    item: Optional[T]

  LoopScope* [S] = object
    condition: Condition[S]
    stepper: Stepper[S]



func condition* [T](predicate: Predicate[T]): Condition[T] =
  predicate


func stepper* [S](reader: Reader[S, S]): Stepper[S] =
  reader



func runOnceResult* [S; T](step: S; item: Optional[T]): RunOnceResult[S, T] =
  RunOnceResult[S, T](step: step, item: item)



template stepType* [S](X: typedesc[LoopScope[S]]): typedesc[S] =
  S


template stepType* [S](self: LoopScope[S]): typedesc[S] =
  self.typeof().stepType()



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



func looped* [S](condition: Condition[S]; stepper: Stepper[S]): LoopScope[S] =
  LoopScope[S](condition: condition, stepper: stepper)


func looped* [S](condition: Condition[S]; stepper: S -> S): LoopScope[S] =
  condition.looped(stepper as Stepper[S])



func emptyLoopScope* (S: typedesc): LoopScope[S] =
  alwaysFalse[S].looped(itself[S])


func infinite* [S](stepper: Stepper[S]): LoopScope[S] =
  alwaysTrue[S].looped(stepper)



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



proc run* [S](self: LoopScope[S]; initial: S; body: S -> Unit): S =
  result = initial

  while self.condition.test(result.read()):
    body.run(result.read()).ignore()
    result.modify(self.stepper).ignore()


proc run* [S](self: LoopScope[S]; initial: S): S =
  ## Runs the loop while doing nothing in its body.
  self.run(initial, doNothing)



func asReader* [S](self: LoopScope[S]; body: S -> Unit): Reader[S, S] =
  partial(self.run(?:S, body))


func asReader* [S](self: LoopScope[S]): Reader[S, S] =
  self.asReader(doNothing)



proc runOnce* [S; T](
  self: LoopScope[S];
  start: S;
  body: S -> T
): RunOnceResult[S, T] =
  self
    .condition
    .ifElse(
      body
        .map((item: T) => runOnceResult(self.stepper.run(start), item.toSome()))
      ,
      partial(runOnceResult(?:S, T.toNone()))
    ).run(start)



proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  conditionMapper: Condition[SA] -> Condition[SB];
  stepperMapper: Stepper[SA] -> Stepper[SB]
): LoopScope[SB] =
  conditionMapper
    .run(self.read(self.typeof().condition()))
    .looped(stepperMapper.run(self.read(self.typeof().stepper())))


proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB]
): LoopScope[SB] =
  self.mapSteps(partial(extractor.map(?:Condition[SA])), stepperMapper)


proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  extractor: SB -> SA;
  wrapper: SA -> SB
): LoopScope[SB] =
  self.mapSteps(extractor, stepper => extractor.map(stepper).map(wrapper))



func breakIf* [S](self: LoopScope[S]; predicate: Predicate[S]): LoopScope[S] =
  self.modify(self.typeof().condition(), partial(?_ and not predicate))



when isMainModule:
  import ../monad/[identity]
  import ../optics/[lenslaws]
  import ../utils/[operators]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"LoopScope[S].condition()" should return a lens that obeys the lens laws.""":
      proc doTest [S](spec: LensLawsSpec[LoopScope[S], Condition[S]]) =
        check:
          LoopScope[S].condition().checkLensLaws(spec)


      proc runTest1 () =
        let scope = partial(?:int32 < 53).looped(plus1)

        doTest(
          lensLawsSpec(
            identitySpec(scope),
            retentionSpec(scope, partial(?:scope.stepType() < 100).condition()),
            doubleWriteSpec(
              scope,
              partial(?:scope.stepType() > 5).condition(),
              partial(?_ != 7)
            )
          )
        )


      runTest1()



    test """"LoopScope[S].stepper()" should return a lens that obeys the lens laws.""":
      proc doTest [S](spec: LensLawsSpec[LoopScope[S], Stepper[S]]) =
        check:
          LoopScope[S].stepper().checkLensLaws(spec)


      proc runTest1 () =
        let scope = partial(?:char in {'a'..'z'}).looped(next)

        doTest(
          lensLawsSpec(
            identitySpec(scope),
            retentionSpec(scope, stepper(prev[scope.stepType()])),
            doubleWriteSpec(
              scope,
              stepper((c: scope.stepType()) => '0'),
              itself[scope.stepType()]
            )
          )
        )


      runTest1()



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
      proc doTest [S; T](spec: LensLawsSpec[RunOnceResult[S, T], Optional[T]]) =
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



    test """Using "sut.run(0, doNothing)" to count up to "expected" should return "expected".""":
      proc doTest [N: SomeUnsignedInt](expected: N) =
        let actual = partial(?:N < expected).looped(plus1[N].stepper()).run(0.N)

        check:
          actual == expected


      doTest(56u8)
      doTest(4u32)
      doTest(0u)
      doTest(1u64)



    test """Using "sut.run(0)" to count up to "expected" at compile time should return "expected".""":
      proc doTest [N: SomeUnsignedInt](expected: static N) =
        const actual =
          partial(?:N < expected)
            .looped(plus1[N].stepper())
            .run(0.N)

        check:
          actual == expected


      doTest(56u8)
      doTest(4u32)
      doTest(0u)
      doTest(1u64)



    test """Using a LoopScope to copy an array should return an array equal to the original.""":
      proc doTest [I: Ordinal; T](expected: array[I, T]) =
        var copy: expected.typeof()

        let sut = looped((i: I) => i.int < expected.len(), next)

        sut
          .run(
            expected.low(),
            proc (i: I): Unit =
              copy[i] = expected[i]
          ).ignore()

        check:
          copy == expected


      doTest(array[0, char].default())
      doTest(["a", "0123", "abc"])



    test """Running an "emptyLoopScope(I)" to count up starting at "start" should return "start".""":
      proc doTest [I: SomeInteger](start: I) =
        let
          expected = start
          actual = I.emptyLoopScope().run(start)

        check:
          actual == expected


      doTest(-3)
      doTest(int.low())
      doTest(Natural.high())



    when defined(js):
      #[
        Disable the test because JS maps Nim's integer types to a "Number" or
        "BigInt" and it could potentially last for a while.
      ]#
      discard
    else:
      test """Running an "infinite(next[I])" to count up starting at "start" should raise an "OverflowError".""":
        proc doTest [I: SomeSignedInt](start: I) =
          debugEcho(start)
          expect OverflowError:
            next[I].infinite().run(start).ignore()


        doTest(int16.high() div 2.int16)
        doTest(int8.high())
        doTest(0.int8)



    test """Using "sut.runOnce(?, ?)" on an infinite LoopScope should enter the loop only once.""":
      proc doTest () =
        let
          actual = alwaysTrue[Natural].looped(plus1).runOnce(0, doNothing)
          expected = runOnceResult(1.Natural, unit().toSome())

        check:
          actual == expected


      doTest()



    test """Using "sut.runOnce(?, ?)" on an empty LoopScope should never enter the loop.""":
      proc doTest () =
        let
          actual = alwaysFalse[Natural].looped(plus1).runOnce(0, doNothing)
          expected = runOnceResult(0.Natural, Unit.toNone())

        check:
          actual == expected


      doTest()



    test """Using "sut.breakIf(n => n mod 2 == 1)" when iterating on Natural numbers should return the first found odd number.""":
      func isOdd [I: SomeInteger](i: I): bool =
        i mod 2 == 1


      proc doTest (start: Natural) =
        let
          actual =
            partial(?:start.typeof() < 10)
              .looped(next)
              .breakIf(isOdd[start.typeof()])
              .run(start)
          expected = isOdd[start.typeof()].ifElse(itself, next).run(start)

        check:
          actual == expected


      doTest(0)
      doTest(Natural.high())
