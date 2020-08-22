##[
  Polymorphic lenses from functional programming.

  A lens lets one focus on a subpart of a whole (typically a data structure),
  and manipulate that subpart while keeping the rest of the structure.

  A lens is an abstraction of a structure member.
  Lenses can be chained together to let one see further in the focused
  structure.

  `Normal lenses <lens.html>`_ let one modify a structure without changing its
  type.
  Polymorphic lenses allow to do so while changing its type.

  Modifications of the focused structure can be free of side effects or not. It
  is up to the lens implementation.

  Examples:
    - Define polymorphic lenses for a structure type.

      See `block <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/nim_iterator_stream_experiment/optics/plens/private/test/block.nim>`_ and `conditionalblock <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/nim_iterator_stream_experiment/optics/plens/private/test/conditionalblock.nim>`_.

    - Using polymorphic lenses to `modify parts of a structure <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/examples/optics/plens/modify.nim>`_.
]##



import ../monad/[reader]

import std/[sugar]



export reader



type
  MemberReader* [S; T] = Reader[S, T]
  MemberUpdater* [SR; W; SW] = (state: SR, value: W) -> SW

  PLens* [SR; R; W; SW] = object
    reader: MemberReader[SR, R]
    writer: MemberUpdater[SR, W, SW]



template readStateType* [SR; R; W; SW](
  X: typedesc[PLens[SR, R, W, SW]]
): typedesc[SR] =
  SR


template readMemberType* [SR; R; W; SW](
  X: typedesc[PLens[SR, R, W, SW]]
): typedesc[R] =
  R


template writtenMemberType* [SR; R; W; SW](
  X: typedesc[PLens[SR, R, W, SW]]
): typedesc[W] =
  W


template writtenStateType* [SR; R; W; SW](
  X: typedesc[PLens[SR, R, W, SW]]
): typedesc[SW] =
  SW



template readStateType* [SR; R; W; SW](
  self: PLens[SR, R, W, SW]
): typedesc[SR] =
  self.typeof().readStateType()


template readMemberType* [SR; R; W; SW](
  self: PLens[SR, R, W, SW]
): typedesc[R] =
  self.typeof().readArgType()


template writtenMemberType* [SR; R; W; SW](
  self: PLens[SR, R, W, SW]
): typedesc[W] =
  self.typeof().writtenMemberType()


template writtenStateType* [SR; R; W; SW](
  self: PLens[SR, R, W, SW]
): typedesc[SW] =
  self.typeof().writtenStateType()



func lens* [SR; R; W; SW](
  reader: MemberReader[SR, R];
  writer: MemberUpdater[SR, W, SW]
): PLens[SR, R, W, SW] =
  PLens[SR, R, W, SW](reader: reader, writer: writer)



func read* [SR; R; W; SW](self: PLens[SR, R, W, SW]): Reader[SR, R] =
  self.reader


func write* [SR; R; W; SW](
  self: PLens[SR, R, W, SW];
  value: () -> W
): Reader[SR, SW] =
  (state: SR) => self.writer(state, value())


func modify* [SR; R; W; SW](
  self: PLens[SR, R, W, SW];
  f: R -> W
): Reader[SR, SW] =
  self.read().map(f).flatMap((w: W) => self.write(() => w))



func chain* [SR; R1; W1; SW; R2; W2](
  self: PLens[SR, R1, W1, SW];
  other: PLens[R1, R2, W2, W1]
): PLens[SR, R2, W2, SW] =
  lens(
    self.read().map(other.read()),
    (state: SR, value: W2) =>
      self.modify(other.write(() => value)).run(state)
  )



proc read* [SR; R; W; SW](state: SR; lens: PLens[SR, R, W, SW]): R =
  lens.read().run(state)


proc write* [SR; R; W; SW](
  state: SR;
  lens: PLens[SR, R, W, SW];
  value: W
): SW =
  lens.write(() => value).run(state)


proc modify* [SR; R; W; SW](
  state: SR;
  lens: PLens[SR, R, W, SW];
  f: R -> W
): SW =
  lens.modify(f).run(state)



when isMainModule:
  import plens/private/test/["block", conditionalblock]
  import ../utils/[call]

  import std/[os, unittest]



  type Lens [S; T] = PLens[S, T, T, S]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """Reading the condition of a "ConditionalBlock[T]" through a lens should return the same value as the member of the structure.""":
        proc doTest [T](
          lens: Lens[ConditionalBlock[T], Condition];
          input: ConditionalBlock[T]
        ) =
          let
            actual = lens.read().run(input)
            expected = input.condition()

          check:
            actual == expected


        doTest(
          ConditionalBlock[int].condition(),
          conditionalBlock(() => false, `block`("", () => 0))
        )



      test """Modifying the body of a "ConditionalBlock[T]" through a lens should return a "ConditionalBlock[T]" with only the body modified.""":
        proc doTest [A; B](
          lens:
            PLens[
              ConditionalBlock[A],
              BlockBody[A],
              BlockBody[B],
              ConditionalBlock[B]
            ]
          ;
          modifier: BlockBody[A] -> BlockBody[B];
          input: ConditionalBlock[A]
        ) =
          # We run the modifier once so that we can manipulate the same closure.
          let
            modifiedBody = lens.read().map(modifier).run(input)
            actual = lens.modify(_ => modifiedBody).run(input)
            expected =
              conditionalBlock(
                input.condition(),
                `block`(input.thenLabel(), modifiedBody)
              )

          check:
            actual == expected


        doTest(
          ConditionalBlock[int].thenBody(string),
          body => (() => $body.call()),
          conditionalBlock(() => true, `block`("abc", () => 1))
        )



      test """Writing the label of a "ConditionalBlock[T]" through a lens should return a "ConditionalBlock[T]" with only the label modified.""":
        proc doTest [T](
          lens: Lens[ConditionalBlock[T], BlockLabel];
          written: BlockLabel;
          input: ConditionalBlock[T]
        ) =
          let
            actual = lens.write(() => written).run(input)
            expected =
              conditionalBlock(
                input.condition(),
                `block`(written, input.thenBlock().body())
              )

          check:
            actual == expected


        doTest(
          ConditionalBlock[char].thenLabel(),
          "0123",
          conditionalBlock(() => false, `block`("a", () => 'a'))
        )



      test """Modifying the body of a "ConditionalBlock[T]" through a lens at compile time should return a "ConditionalBlock[T]" with only the body modified.""":
        # Skipped because of https://github.com/nim-lang/Nim/issues/9679.
        when true:
          skip()
        else:
          proc doTest [A; B](
            lens:
              static[
                proc ():
                  PLens[
                    ConditionalBlock[A],
                    BlockBody[A],
                    BlockBody[B],
                    ConditionalBlock[B]
                  ]
                  {.nimcall, noSideEffect.}
              ]
            ;
            modifier:
              static[
                proc (input: BlockBody[A]): BlockBody[B] {.
                  nimcall,
                  noSideEffect
                .}
              ]
            ;
            input: static proc (): ConditionalBlock[A] {.nimcall, noSideEffect.}
          ) =
            const
              modifiedBody = lens.call().read().map(modifier).run(input.call())
              actual = lens.call().modify(_ => modifiedBody).run(input.call())
              expected =
                conditionalBlock(
                  input.call().condition(),
                  `block`(input.call().thenLabel(), modifiedBody)
                )

            check:
              actual == expected


          func modifier (input: BlockBody[int]): BlockBody[string] =
            () => $input.call()


          doTest(
            () => ConditionalBlock[int].thenBody(string),
            modifier,
            () => conditionalBlock(() => true, `block`("abc", () => 1))
          )



  main()
