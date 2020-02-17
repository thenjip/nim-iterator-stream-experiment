import loop/[loopedcondition, loopstates]
import monad/[identity, reader]
import utils/[chain, convert, ignore, predicate, unit]

import std/[sugar]



type
  Generator* [S; T] = Reader[S, T]

  Loop* [S; T] = object
    condition: LoopedCondition[S]
    generator: Generator[S, T]



func generating* [S; T](
  condition: LoopedCondition[S];
  generator: Generator[S, T]
): Loop[S, T] =
  Loop[S, T](condition: condition, generator: generator)


func emptyLoop* (T: typedesc): Loop[EmptyState, T] =
  func buildLoop (S, T: typedesc): Loop[S, T] =
    alwaysFalse[S].looped(itself[S]).generating(nil as Generator[S, T])

  EmptyState.buildLoop(T)


func emptyLoop* [T](): Loop[EmptyState, T] =
  T.emptyLoop()


func singleItemLoop* [T](value: () -> T): Loop[SingleItemState, T] =
  func buildLoop [T](S: typedesc; value: () -> T): Loop[S, T] =
    alwaysTrue[S]
      .looped(alwaysFalse[S].chain(singleItemState))
      .generating((_: S) => value())

  SingleItemState.buildLoop(value)


func infiniteLoop* [S; T](
  generator: Generator[S, T];
  nextStep: Stepper[S]
): Loop[S, T] =
  alwaysTrue[S].looped(nextStep).generating(generator)



proc hasMore [S; T](self: Loop[S, T]; current: S): bool =
  self.condition.hasMore(current)


proc nextStep [S; T](self: Loop[S, T]; current: S): S =
  self.condition.nextStep(current)


proc generate [S; T](self: Loop[S, T]; current: S): T =
  self.generator.run(current)



proc run* [S](self: Loop[S, Unit]; initial: S): S =
  ##[
    Runs the `Loop` and returns the final step.
  ]##
  result = initial

  while self.hasMore(result):
    self.generate(result).ignore()
    result = self.nextStep(result)


func asReader* [S](self: Loop[S, Unit]): Reader[S, S] =
  (initial: S) => self.run(initial)



func map* [S; A; B](self: Loop[S, A]; f: A -> B): Loop[S, B] =
  self.condition.generating(self.generator.map(f))



func breakIf* [S; T](
  self: Loop[S, T];
  condition: LoopCondition[S]
): Loop[S, T] =
  itself((step: S) => self.hasMore(step))
    .`and`(not condition)
    .looped((step: S) => self.nextStep(step))
    .generating(self.generator)


func skipIf* [S; T](self: Loop[S, T]; condition: Predicate[T]): Loop[S, T] =
  discard



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      int
        .emptyLoop()
        .map(_ => unit())
        .breakIf(alwaysFalse[EmptyState])
        .asReader()
        .run(emptyState())
        .ignore()
