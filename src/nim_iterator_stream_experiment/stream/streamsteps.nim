import ../monad/[optional]
import ../optics/[lens]

import std/[sugar]



type
  LimitStep* [S; N: SomeUnsignedInt] = object
    step: S
    count: N

  TakeWhileStep* [S; T] = object
    step: S
    item: Optional[T]

  SkipStep* [S; N: SomeUnsignedInt] = object
    step: S
    count: N



func limitStep* [S; N](step: S; count: N): LimitStep[S, N] =
  LimitStep[S, N](step: step, count: count)


func step* [S; N](X: typedesc[LimitStep[S, N]]): Lens[X, S] =
  lens((self: X) => self.step, (self: X, step: S) => step.limitStep(self.count))


func count* [S; N](X: typedesc[LimitStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => self.step.limitStep(count)
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



func skipStep* [S; N](step: S; count: N): SkipStep[S, N] =
  SkipStep[S, N](step: step, count: count)


func step* [S; N](X: typedesc[SkipStep[S, N]]): Lens[X, S] =
  lens((self: X) => self.step, (self: X, step: S) => step.skipStep(self.count))


func count* [S; N](X: typedesc[SkipStep[S, N]]): Lens[X, N] =
  lens(
    (self: X) => self.count,
    (self: X, count: N) => self.step.skipStep(count)
  )
