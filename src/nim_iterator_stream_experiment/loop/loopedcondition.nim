import ../monad/[reader]
import ../optics/[focus, lens]
import ../utils/[convert, ignore, predicate, unit]

import std/[sugar]



type
  Condition* [S] = Predicate[S]
  Stepper* [S] = Reader[S, S]

  LoopedCondition* [S] = object
    condition: Condition[S]
    stepper: Stepper[S]



func looped* [S](
  condition: Condition[S];
  stepper: Stepper[S]
): LoopedCondition[S] =
  LoopedCondition[S](condition: condition, stepper: stepper)


func looped* [S](
  condition: Predicate[S];
  stepper: Stepper[S]
): LoopedCondition[S] =
  condition.convert(Condition[S]).looped(stepper)



func condition* [S](X: typedesc[LoopedCondition[S]]): Lens[X, Condition[S]] =
  lens(
    (self: X) => self.condition,
    (self: X, condition: Condition[S]) => condition.looped(self.stepper)
  )


func stepper* [S](X: typedesc[LoopedCondition[S]]): Lens[X, Stepper[S]] =
  lens(
    (self: X) => self.stepper,
    (self: X, stepper: Stepper[S]) => self.condition.looped(stepper)
  )



func wrapSteps* [SA; SB](
  self: LoopedCondition[SA];
  extractor: SB -> SA;
  stepperBuilder: Stepper[SA] -> Stepper[SB]
): LoopedCondition[SB] =
  func buildResult [X: self.typeof()](
    self: X;
    extractor: extractor.typeof();
    stepperBuilder: stepperBuilder.typeof();
    SB: typedesc
  ): result.typeof() =
      extractor
        .map(self.condition)
        .convert(Condition[SB])
        .looped(stepperBuilder.run(self.stepper))

  self.buildResult(extractor, stepperBuilder, SB)



proc advance* [S](stepper: Stepper[S]; current: S): S =
  stepper.run(current)



proc run* [S](self: LoopedCondition[S]; initial: S; body: S -> Unit): S =
  result = initial

  while self.condition.test(result):
    body.run(result).ignore()
    result = self.stepper.advance(result)


proc run* [S](self: LoopedCondition[S]; initial: S): S =
  self.run(initial, doNothing)


proc asReader* [S](self: LoopedCondition[S]; body: S -> Unit): Reader[S, S] =
  (initial: S) => self.run(initial, body)


proc asReader* [S](self: LoopedCondition[S]): Reader[S, S] =
  self.asReader(doNothing)



func breakIf* [S](
  self: LoopedCondition[S];
  isBroken: Predicate[S]
): LoopedCondition[S] =
  self
    .focusOn(self.typeof().condition())
    .modify(hasMore => hasMore and not isBroken)



when isMainModule:
  import ../monad/[identity]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      alwaysFalse[Unit].looped(itself[Unit]).asReader().run(unit()).ignore()
