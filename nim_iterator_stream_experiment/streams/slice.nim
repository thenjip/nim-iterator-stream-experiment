import ../stream
import ../stream/loop
import ../stream/loop/[loopscope]
import ../monad/[reader]
import ../optics/[lens]
import ../utils/[convert, nimnodes, operators, partialprocs]

import std/[sugar]



export stream



type
  SliceStepSignedIndex* = BiggestInt

  SliceStep* [T] = object
    i: T

  SliceStepSigned* = SliceStep[SliceStepSignedIndex]



func slice* [T](low, high: T): Slice[T] =
  low .. high


func low* [T](self: Slice[T]): T =
  self.a


func high* [T](self: Slice[T]): T =
  self.b



func sliceStep* [T](i: T): SliceStep[T] =
  SliceStep[T](i: i)


func sliceStepSigned* [T: Ordinal](i: T): SliceStepSigned =
  sliceStep(i as SliceStepSignedIndex)


func current* [T](S: typedesc[SliceStep[T]]): Lens[S, T] =
  lens((self: S) => self.i, (_: S, i: T) => sliceStep(i))



func ordinals* [T: Ordinal](s: Slice[T]): Stream[SliceStepSigned, T] =
  let
    lens = result.typeof().stepType().current()
    readCurrent = lens.read()

  readCurrent
    .map(partial(?_ < s.high().convert(SliceStepSignedIndex).next()))
    .looped(lens.modify(next))
    .generating(readCurrent.map(partial(?_ as T)))
    .startingAt(() => sliceStepSigned(s.low()))


func items* [T: Ordinal](s: Slice[T]): Stream[SliceStep[T], T] =
  let
    lens = SliceStep[T].current()
    readCurrent = lens.read()

  readCurrent
    .map(partial(?_ < s.high().next()))
    .looped(lens.modify(next))
    .generating(readCurrent)
    .startingAt(() => sliceStep(s.low()))



when isMainModule:
  import ../utils/[ignore]

  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """Counting the number of values in "char.low() .. char.high()" with "ordinals()" should return the length of this slice.""":
        proc doTest () =
          let
            slice = char.low() .. char.high()
            actual = slice.ordinals().count(Natural)
            expected = slice.len()

          check:
            actual == expected


        doTest()



      test """Counting the number of values in the "char" type with "items()" should raise an "OverflowError".""":
        proc doTest () =
          when defined(js):
            # The generated JS code does not seem to raise an "OverflowError".
            skip()
          else:
            expect OverflowError:
              slice(char.low(), char.high()).items().count(Natural).ignore()


        doTest()



      test """Counting the number of items in an empty slice should return 0.""":
        proc doTest [T: Ordinal](slice: Slice[T]) =
          let
            actual = slice.items().count(BiggestUInt)
            expected = slice.len() as actual.typeof()

          require:
            expected == 0

          check:
            actual == expected


        doTest(-1i32 .. -53i32)
        doTest(156u64 .. 1u64)



      test """Counting the number of odd numbers in an integer slice "s" at compile time should return "s.len() div 2".""":
        when defined(js):
          skip()
        else:
          func isOdd [I: SomeInteger](i: I): bool =
            i mod 2 == 1


          proc doTest [I: SomeInteger](low, high: static I) =
            const
              s = slice(low, high)
              actual = s.items().filter(isOdd[I]).count(BiggestUInt)
              expected = s.len() div 2 as actual.typeof()

            check:
              actual == expected


          doTest(0u, 10u)



    main()
