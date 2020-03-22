import loop/[loopedcondition, loopsteps]
import monad/[identity, io, reader]
import optics/[lens]
import utils/[convert, predicate, unit]

import std/[sugar]



type
  Generator* [S; T] = Reader[S, T]

  Loop* [S; T] = object
    loopedCondition: LoopedCondition[S]
    generator: Generator[S, T]



func generating* [S; T](
  loopedCondition: LoopedCondition[S];
  generator: Generator[S, T]
): Loop[S, T] =
  Loop[S, T](loopedCondition: loopedCondition, generator: generator)



func loopedCondition* [S; T](
  X: typedesc[Loop[S, T]]
): Lens[X, LoopedCondition[S]] =
  lens(
    (self: X) => self.loopedCondition,
    (self: X, loopedCondition: LoopedCondition[S]) =>
      loopedCondition.generating(self.generator)
  )


func generator* [S; A](
  X: typedesc[Loop[S, A]];
  B: typedesc
): PLens[X, Generator[S, A], Generator[S, B], Loop[S, B]] =
  lens(
    (self: X) => self.generator,
    (self: X, generator: Generator[S, B]) =>
      self.loopedCondition.generating(generator)
  )


func generator* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Generator[S, T]] =
  X.generator(T)


func condition* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Condition[S]] =
  X.loopedCondition().chain(LoopedCondition[S].condition())


func stepper* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Stepper[S]] =
  X.loopedCondition().chain(LoopedCondition[S].stepper())



func wrapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  stepperBuilder: Stepper[SA] -> Stepper[SB]
): Loop[SB, T] =
  self
    .loopedCondition
    .wrapSteps(extractor, stepperBuilder)
    .generating(extractor.map(self.generator))



func emptyLoop* (T: typedesc): Loop[ZeroStep, T] =
  func buildLoop (S: typedesc; T: typedesc): result.typeof() =
    alwaysFalse[S]
      .looped(itself[S])
      .generating(nil as Generator[S, T])

  ZeroStep.buildLoop(T)


func infiniteLoop* [S; T](
  generator: Generator[S, T];
  nextStep: Stepper[S]
): Loop[S, T] =
  alwaysTrue[S].looped(nextStep).generating(generator)



proc run* [S](self: Loop[S, Unit]; initial: S): S =
  ##[
    Runs the `Loop` and returns the step after the last valid one.
  ]##
  self.loopedCondition.run(initial, self.generator)


func asReader* [S](self: Loop[S, Unit]): Reader[S, S] =
  (initial: S) => self.run(initial)



func map* [S; A; B](self: Loop[S, A]; f: A -> B): Loop[S, B] =
  self.loopedCondition.generating(self.generator.map(f))



when isMainModule:
  import optics/[focus]
  import utils/[ignore]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      int
        .emptyLoop()
        .map(_ => unit())
        .apply(
          loop =>
            loop
              .focusOn(loop.typeof().loopedCondition())
              .modify(loopedCond => loopedCond.breakIf(alwaysFalse[ZeroStep]))
        ).run(zeroStep())
        .ignore()
