import loop/[loopscope, loopsteps]
import monad/[identity, io, optional, predicate, reader]
import optics/[focus, lens]
import utils/[convert, partialprocs, unit]

import std/[sugar]



type
  Generator* [S; T] = Reader[S, T]

  Loop* [S; T] = object
    scope: LoopScope[S]
    generator: Generator[S, T]



func generator* [S; T](g: Reader[S, T]): Generator[S, T] =
  g



func generating* [S; T](
  scope: LoopScope[S];
  generator: Generator[S, T]
): Loop[S, T] =
  Loop[S, T](scope: scope, generator: generator)



func scope* [S; T](X: typedesc[Loop[S, T]]): Lens[X, LoopScope[S]] =
  lens(
    (self: X) => self.scope,
    (self: X, scope: LoopScope[S]) => scope.generating(self.generator)
  )


func generator* [S; A](
  X: typedesc[Loop[S, A]];
  B: typedesc
): PLens[X, Generator[S, A], Generator[S, B], Loop[S, B]] =
  lens(
    (self: X) => self.generator,
    (self: X, generator: Generator[S, B]) => self.scope.generating(generator)
  )


func generator* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Generator[S, T]] =
  X.generator(T)


func condition* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Condition[S]] =
  X.scope().chain(LoopScope[S].condition())


func stepper* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Stepper[S]] =
  X.scope().chain(LoopScope[S].stepper())



proc mapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB]
): Loop[SB, T] =
  self
    .scope
    .mapSteps(extractor, stepperMapper)
    .generating(extractor.map(self.generator))


proc mapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  wrapper: SA -> SB
): Loop[SB, T] =
  self.mapSteps(extractor, stepper => extractor.map(stepper).map(wrapper))



func emptyLoop* (T: typedesc): Loop[ZeroStep, T] =
  func buildResult (S: typedesc; T: typedesc): result.typeof() =
    alwaysFalse[S]
      .looped(itself[S])
      .generating(nil as Generator[S, T])

  ZeroStep.buildResult(T)


func infiniteLoop* [S; T](
  generator: Generator[S, T];
  stepper: Stepper[S]
): Loop[S, T] =
  alwaysTrue[S].looped(stepper).generating(generator)



proc run* [S](self: Loop[S, Unit]; initial: S): S =
  self.scope.run(initial, self.generator)



func asReader* [S](self: Loop[S, Unit]): Reader[S, S] =
  self.scope.asReader(self.generator)



proc runOnce* [S; T](self: Loop[S, T]; start: S): RunOnceResult[S, T] =
  self.scope.runOnce(start, self.generator)



func map* [S; A; B](self: Loop[S, A]; f: A -> B): Loop[S, B] =
  self.scope.generating(self.generator.map(f))



func takeWhile* [S; T](
  self: Loop[S, T];
  predicate: Predicate[T]
): Loop[TakeWhileStep[S, T], T] =
  func buildResult [X: self.typeof(); S; T](
    self: X;
    predicate: predicate.typeof();
    TW: typedesc[TakeWhileStep[S, T]]
  ): result.typeof() =
    let
      readStep = TW.step().read()
      readItem = TW.item().read()

    self
      .mapSteps(
        readStep,
        (_: Stepper[S]) =>
          readStep
            .map(partial(self.runOnce(?_)))
            .map(
              (generated: RunOnceResult[S, T]) =>
                takeWhileStep(
                  generated.read(generated.typeof().step()),
                  generated.read(generated.typeof().item()).filter(predicate)
                )
            )
      ).write(result.typeof().condition(), readItem.map(isSome))
      .write(result.typeof().generator(), readItem.map(get))

  self.buildResult(predicate, TakeWhileStep[S, T])


func dropWhile* [S; T](
  self: Loop[S, T];
  predicate: Predicate[T]
): Reader[S, S] =
  let scope = self.scope

  scope
    .modify(scope.typeof().condition(),
      partial(?_ and self.generator.map(predicate))
    ).asReader()



when isMainModule:
  import optics/[lenslaws]
  import utils/[ignore]

  import std/[os, sequtils, unittest]



  suite currentSourcePath().splitFile().name:
    test """"Loop[S, T].scope()" should return a lens that obeys the lens laws.""":
      proc doTest [S; T](spec: LensLawsSpec[Loop[S, T], LoopScope[S]]) =
        check:
          Loop[S, T].scope().checkLensLaws(spec)


      proc runTest1 () =
        let loop = infiniteLoop((_: Unit) => 14u64, itself[Unit].stepper())

        proc doRun (S: typedesc[loop.typeof().S]) =
          doTest(
            lensLawsSpec(
              identitySpec(loop),
              retentionSpec(loop, alwaysFalse[S].looped(itself)),
              doubleWriteSpec(
                loop,
                alwaysFalse[S].looped(itself),
                alwaysTrue[S].looped(itself)
              )
            )
          )

        doRun(loop.typeof().S)


      runTest1()



    test """"Loop[S, T].generator()" should return a lens that obeys the lens laws.""":
      proc doTest [S; T](spec: LensLawsSpec[Loop[S, T], Generator[S, T]]) =
        check:
          Loop[S, T].generator().checkLensLaws(spec)


      proc runTest1 () =
        let loop = seq[Unit].emptyLoop()

        proc doRun (
          S: typedesc[loop.typeof().S];
          T: typedesc[loop.typeof().T]
        ) =
          doTest(
            lensLawsSpec(
              identitySpec(loop),
              retentionSpec(loop, generator((_: S) => unit().repeat(5))),
              doubleWriteSpec(
                loop,
                generator((_: S) => @[].T),
                (_: S) => @[unit()]
              )
            )
          )

        doRun(loop.typeof().S, loop.typeof().T)


      runTest1()



    test "test":
      int
        .emptyLoop()
        .map(_ => unit())
        .apply(
          loop =>
            loop.modify(
              loop.typeof().scope(),
              scope => scope.breakIf(alwaysFalse[ZeroStep])
            )
        ).run(zeroStep())
        .ignore()
