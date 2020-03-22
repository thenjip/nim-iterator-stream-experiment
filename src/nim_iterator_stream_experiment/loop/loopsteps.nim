import ../optics/[lens]

import std/[sugar]



type
  ZeroStep* = object
    discard

  SingleStep* = object
    consumed: bool



func zeroStep* (): ZeroStep =
  ZeroStep()



func singleStep* (consumed: bool): SingleStep =
  SingleStep(consumed: consumed)


func isConsumed* (T: typedesc[SingleStep]): Lens[T, bool] =
  lens(
    (self: T) => self.consumed,
    (_: T, consumed: bool) => consumed.singleStep()
  )
