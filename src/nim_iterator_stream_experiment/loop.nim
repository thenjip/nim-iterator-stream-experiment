import loop/[loopscope, loopsteps]
import monad/[identity, io, optional, predicate, reader]
import optics/[focus, lens]
import utils/[convert, unit]

import std/[sugar]



type
  Generator* [S; T] = Reader[S, T]

  Loop* [S; T] = object
    loopScope: LoopScope[S]
    generator: Generator[S, T]



func generating* [S; T](
  loopScope: LoopScope[S];
  generator: Generator[S, T]
): Loop[S, T] =
  Loop[S, T](loopScope: loopScope, generator: generator)



func loopScope* [S; T](X: typedesc[Loop[S, T]]): Lens[X, LoopScope[S]] =
  lens(
    (self: X) => self.loopScope,
    (self: X, loopScope: LoopScope[S]) => loopScope.generating(self.generator)
  )


func generator* [S; A](
  X: typedesc[Loop[S, A]];
  B: typedesc
): PLens[X, Generator[S, A], Generator[S, B], Loop[S, B]] =
  lens(
    (self: X) => self.generator,
    (self: X, generator: Generator[S, B]) =>
      self.loopScope.generating(generator)
  )


func generator* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Generator[S, T]] =
  X.generator(T)


func condition* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Condition[S]] =
  X.loopScope().chain(LoopScope[S].condition())


func stepper* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Stepper[S]] =
  X.loopScope().chain(LoopScope[S].stepper())



proc mapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB]
): Loop[SB, T] =
  self
    .loopScope
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



proc run* [S](self: Loop[S, Unit]; initial: () -> S): S =
  self.loopScope.run(initial, self.generator)


func asReader* [S](self: Loop[S, Unit]): Reader[S, S] =
  (initial: S) => self.run(initial)



func map* [S; A; B](self: Loop[S, A]; f: A -> B): Loop[S, B] =
  self.loopScope.generating(self.generator.map(f))


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
      generateWrappedStep =
        (step: S) =>
          step.takeWhileStep(
            self.read(X.condition()).test(step).ifElse(
              () => self.generator.run(step).toSome().filter(predicate),
              toNone[T]
            )
          )

    self
      .mapSteps(
        readStep,
        (stepper: Stepper[S]) => readStep.map(stepper).map(generateWrappedStep)
      ).write(result.typeof().condition(), readItem.map(isSome))
      .write(result.typeof().generator(), readItem.map(get))

  self.buildResult(predicate, TakeWhileStep[S, T])



when isMainModule:
  import utils/[ignore]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      int
        .emptyLoop()
        .map(_ => unit())
        .apply(
          loop =>
            loop.modify(
              loop.typeof().loopScope(),
              loopedCond => loopedCond.breakIf(alwaysFalse[ZeroStep])
            )
        ).run(zeroStep)
        .ignore()
