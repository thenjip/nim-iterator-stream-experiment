import ../monad/[reader]
import ../utils/[predicate]



type
  LoopCondition* [T] = Predicate[T]
  Stepper* [T] = Reader[T, T]

  LoopedCondition* [T] = object
    condition: LoopCondition[T]
    stepper: Stepper[T]



func looped* [T](
  condition: LoopCondition[T];
  stepper: Stepper[T]
): LoopedCondition[T] =
  LoopedCondition[T](condition: condition, stepper: stepper)



proc hasMore* [T](self: LoopedCondition[T]; item: T): bool =
  self.condition.test(item)



proc advance* [T](stepper: Stepper[T]; current: T): T =
  stepper.run(current)


proc nextStep* [T](self: LoopedCondition[T]; current: T): T =
  self.stepper.advance(current)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
