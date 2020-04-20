import ../monad/[optional]
import ../optics/[lens]

import std/[sugar]



type
  ZeroStep* = object
    discard

  SingleStep* = object
    consumed: bool


  TakeWhileStep* [S; T] = object
    step: S
    item: Optional[T]


func zeroStep* (): ZeroStep =
  ZeroStep()



func singleStep* (consumed: bool): SingleStep =
  SingleStep(consumed: consumed)


func isConsumed* (T: typedesc[SingleStep]): Lens[T, bool] =
  lens(
    (self: T) => self.consumed,
    (_: T, consumed: bool) => consumed.singleStep()
  )



func takeWhileStep* [S; T](step: S; item: Optional[T]): TakeWhileStep[S, T] =
  TakeWhileStep[S, T](step: step, item: item)


func step* [S; T](X: typedesc[TakeWhileStep[S, T]]): Lens[X, S] =
  lens(
    (self: X) => self.step,
    (self: X, step: S) => step.takeWhileStep(self.item)
  )


func item* [S; T](X: typedesc[TakeWhileStep[S, T]]): Lens[X, Optional[T]] =
  lens(
    (self: X) => self.item,
    (self: X, item: Optional[T]) => self.step.takeWhileStep(item)
  )
