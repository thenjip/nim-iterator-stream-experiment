##[
  This module provides a stack implementation based on Nim's `seq[T]`.

  This API is purely functional, free of side effects.
]##



import ../monad/[optional]
import ../optics/[lens]
import ../utils/[ifelse, partialprocs]

import std/[sugar]



type
  List [T] = seq[T]

  SeqStack* [T] = object
    ## Since 0.3.0.
    list: List[T]



func isEmpty [T](self: List[T]): bool =
  self.len() == 0


func tail [T](self: List[T]): Optional[T] =
  self.isEmpty().ifElse(toNone[T], () => self[self.high()].toSome())


func deleteLast [T](self: List[T]): seq[T] =
  self[self.low() .. self.high().pred()]



func seqStack* [T](s: seq[T]): SeqStack[T] =
  ## Since 0.3.0.
  SeqStack[T](list: s)


func seqStack* (T: typedesc): SeqStack[T] =
  ##[
    Returns an empty stack.

    Since 0.3.0.
  ]##
  seqStack[T](@[])


func seqStack* [T](): SeqStack[T] =
  ##[
    Returns an empty stack.

    Since 0.3.0.
  ]##
  T.seqStack()



func list [T](X: typedesc[SeqStack[T]]): Lens[X, List[T]] =
  lens((self: X) => self.list, (_: X, list: List[T]) => seqStack(list))



func len* [T](self: SeqStack[T]): Natural =
  ## Since 0.3.0.
  self.list.len()


func isEmpty* [T](self: SeqStack[T]): bool =
  ## Since 0.3.0.
  self.list.isEmpty()


func top* [T](self: SeqStack[T]): Optional[T] =
  ##[
    If the stack is empty, an empty `Optional` will be returned.

    Since 0.3.0.
  ]##
  self.list.tail()



func push* [T](self: SeqStack[T]; item: T): SeqStack[T] =
  ##[
    Returns the updated stack.

    Since 0.3.0.
  ]##
  self.modify(self.typeof().list(), partial(?_ & item))



func deleteLast [T](self: SeqStack[T]): SeqStack[T] =
  self.modify(self.typeof().list(), deleteLast)


func pop* [T](
  self: SeqStack[T]
): tuple[stack: SeqStack[T]; popped: Optional[T]] =
  ##[
    If the stack is empty, ``result.popped`` will be an empty `Optional`.

    Since 0.3.0.
  ]##
  let top = self.top()

  (stack: top.ifSome(_ => self.deleteLast(), () => self), popped: top)



when isMainModule:
  import ../monad/[identity]
  import ../optics/[lenslaws]

  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"SeqStack[T].list()" should return a lens that verifies the lens laws.""":
        proc doTest [T](
          lens: Lens[SeqStack[T], List[T]];
          spec: LensLawsSpec[SeqStack[T], List[T]]
        ) =
          check:
            lens.checkLensLaws(spec)


        doTest(
          SeqStack[int16].list(),
          lensLawsSpec(
            identitySpec(seqStack(@[0i16])),
            retentionSpec(seqStack(seq[int16].default()), @[1i16, 2, 3]),
            doubleWriteSpec(
              seqStack(@[-1i16, 55]),
              seq[int16].default(),
              @[6i16, 9]
            )
          )
        )



      test """"self.push(item)" should return a stack with "item" at the top.""":
        proc doTest [T](self: SeqStack[T]; item: T) =
          let
            actual = self.push(item).top()
            expected = item.toSome()

          check:
            actual == expected


        doTest(seqStack(@["a", "ab"]), "abc")
        doTest(seqStack((int, string)), (1, ""))



      test """"self.pop()" should return a pair of the same stack and no item when "self" is empty.""":
        proc doTest (T: typedesc) =
          let
            self = seqStack[T](@[])
            actual = self.pop()
            expected = (stack: self, popped: T.toNone())

          check:
            actual == expected


        doTest(char)
        doTest(ref Defect)



      test """"self.pop()" should return a pair of a stack without the popped item and that item when "self" is not empty.""":
        proc doTest [T](self: SeqStack[T]) =
          require:
            not self.isEmpty()

          let
            actual =
              self
                .pop()
                .apply(
                  pair => (len: pair.stack.list.len(), popped: pair.popped)
                )
            expected = (len: self.len() - 1, popped: self.top())

          check:
            actual == expected


        doTest(seqStack(@["a0", "a1"]))
        doTest(seqStack(@[int.low()]))



  main()
