import slice
import ../stream
import ../utils/[convert]

import std/[sugar]



type
  SeqIndex* = csize_t

  SeqStep* = SliceStep[SeqIndex]
  ArrayStep* [I: Ordinal] = SliceStep[I]



func indexes* [T](s: seq[T]): Stream[SeqStep, SeqIndex] =
  slice(s.low() as SeqIndex, s.high() as SeqIndex).items()


func items* [T](s: seq[T]): Stream[SeqStep, T] =
  s.indexes().map(i => s[i])


func pairs* [T](s: seq[T]): Stream[SeqStep, tuple[index: SeqIndex; item: T]] =
  s.indexes().map(i => (i, s[i]))



func indexes* (s: string): Stream[SeqStep, SeqIndex] =
  slice(s.low() as SeqIndex, s.high() as SeqIndex).items()


func chars* (s: string): Stream[SeqStep, char] =
  s.indexes().map(i => s[i])


func pairs* (s: string): Stream[SeqStep, tuple[index: SeqIndex; item: char]] =
  s.indexes().map(i => (i, s[i]))



when isMainModule:
  import ../utils/[ignore, unit]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:

    test "test":
      const evenNumberCount =
        @[0, 1, 5, 7, 8, 13]
          .items()
          .filter((it: int) => it mod 2 == 0)
          .count(Natural)

      check:
        "abc".chars().count(Natural) == 3
        evenNumberCount == 2
