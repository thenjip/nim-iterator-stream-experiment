##[
  A structure that encloses a looped computation between a condition and a
  stepper.
]##



import ../monad/[optional, predicate, reader]
import ../optics/[focus, plens, lens]
import ../utils/[convert, ignore, lambda, partialprocs, unit, variables]

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

  while self.condition.test(result):
    body.run(result).ignore()
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
  self.condition.test(start).ifElse(
    body
      .run(start)
      .apply((item: T) => runOnceResult(self.stepper.run(start), item.toSome()))
      .lambda()
    ,
    () => runOnceResult(start, T.toNone())
  )



proc mapSteps* [SA; SB](
  self: LoopScope[SA];
  conditionMapper: Condition[SA] -> Condition[SB];
  stepperMapper: Stepper[SA] -> Stepper[SB]
): LoopScope[SB] =
  self
    .read(self.typeof().condition())
    .conditionMapper()
    .looped(self.read(self.typeof().stepper()).stepperMapper())


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



func breakIf* [S](self: LoopScope[S]; isBroken: Predicate[S]): LoopScope[S] =
  self.modify(self.typeof().condition(), partial(?_ and not isBroken))



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
        let loopScope = partial(?:int32 < 53).looped(plus1)

        proc doRun (S: typedesc) =
          doTest(
            lensLawsSpec(
              identitySpec(loopScope),
              retentionSpec(loopScope, partial(?:S < 100) as Condition[S]),
              doubleWriteSpec(
                loopScope,
                partial(?:S > 5) as Condition[S],
                partial(?_ != 7)
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
        let loopScope = partial(?:char in {'a'..'z'}).looped(c => c.succ())

        proc doRun (S: typedesc) =
          doTest(
            lensLawsSpec(
              identitySpec(loopScope),
              retentionSpec(loopScope, stepper((c: S) => c.pred())),
              doubleWriteSpec(loopScope, stepper((c: S) => '0'), itself[S])
            )
          )

        doRun(loopScope.typeof().S)


      runTest1()



    test """Using "sut.run(0, doNothing)" to count up to "expected" should return "expected".""":
      proc doTest [N: SomeUnsignedInt](expected: N) =
        let sut = partial(?:N < expected).looped(plus1[N])

        check:
          sut.run(0.N, doNothing) == expected


      doTest(100)
      doTest(56u8)
      doTest(4u32)
      doTest(0)
      doTest(1u64)



    test """Using "sut.run(initial, body)" to copy an array should return an array equal to the original.""":
      proc doTest [I: Ordinal, T](expected: array[I, T]) =
        var copy: expected.typeof()

        let sut = partial(?:I < expected.len()).looped(i => i.succ())

        sut
          .run(expected.low(), proc (i: I): Unit = copy[i] = expected[i])
          .ignore()

        check:
          copy == expected


      doTest(array[0, char].default())
      doTest(["a", "0123", "abc"])
