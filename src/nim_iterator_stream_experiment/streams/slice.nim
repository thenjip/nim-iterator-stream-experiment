import ../loop, ../stream
import ../loop/[loopscope]
import ../monad/[reader]
import ../optics/[lens]
import ../utils/[convert, nimnodes, operators, partialprocs]

import std/[sugar]



type
  SmallOrdinal* {.explain.} = concept type T
    T is Ordinal
    T.sizeof() < BiggestInt.default().sizeof()
  BigOrdinal* {.explain.} = concept type T
    T is Ordinal
    T.sizeof() == BiggestInt.default().sizeof()

  SmallOrdSliceStepIndex* = BiggestInt

  SliceStep* [T] = object
    i: T
  SmallOrdSliceStep* = SliceStep[SmallOrdSliceStepIndex]



func slice* [T](low, high: T): Slice[T] =
  low .. high


func low* [T](self: Slice[T]): T =
  self.a


func high* [T](self: Slice[T]): T =
  self.b



func sliceStep* [T](i: T): SliceStep[T] =
  SliceStep[T](i: i)


func smallOrdSliceStep* (i: SmallOrdSliceStepIndex): SmallOrdSliceStep =
  sliceStep(i)


func current* [T](S: typedesc[SliceStep[T]]): Lens[S, T] =
  lens((self: S) => self.i, (_: S, i: T) => sliceStep(i))


func ordinals* [T: SmallOrdinal](s: Slice[T]): Stream[SmallOrdSliceStep, T] =
  let
    lens = SmallOrdSliceStep.current()
    readCurrent = lens.read()

  readCurrent
    .map(partial(?_ < s.high().convert(SmallOrdSliceStepIndex).next()))
    .looped(lens.modify(next))
    .generating(readCurrent.map(partial(?_ as T)))
    .startingAt(() => smallOrdSliceStep(s.low() as SmallOrdSliceStepIndex))


func items* [T: BigOrdinal](s: Slice[T]): Stream[SliceStep[T], T] =
  let
    lens = SliceStep[T].current()
    readCurrent = lens.read()

  readCurrent
    .map(partial(?_ < s.high().next()))
    .looped(lens.modify(next))
    .generating(readCurrent)
    .startingAt(() => sliceStep(s.low()))



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      check:
        slice(char.low(), char.high()).ordinals().count(BiggestUInt) == 256u16
        slice(0u64, 9u64).items().count(csize_t) == 10u
        slice(-1i32, -2).ordinals().count(uint32) == 0
        slice(-100i16, -10).ordinals().count(uint16) == 91
