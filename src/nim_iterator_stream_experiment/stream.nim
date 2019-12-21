import identity, optional, unit, utils

import std/[sugar]



type
  Stream* [S; T] = object
    initialState: () -> S
    hasMore: S -> bool
    generate: S -> T
    nextStep: S -> S

  FilteredStream* [S; T] = Stream[S, Optional[T]]

  FlatStream* [S1; S2; T] = Stream[S1, Stream[S2, T]]

  SingleElementState* = object
    hasMore: bool

  LimitState* [S; U: SomeUnsignedInt] = object
    state: S
    i: U

  TakeWhileState* [S; T] = object
    state: S
    item: Optional[T]



func singleElementState (hasMore: bool): SingleElementState =
  SingleElementState(hasMore: hasMore)


func limitState [S; U: SomeUnsignedInt](state: S; i: U): LimitState[S, U] =
  LimitState[S, U](state: state, i: i)


proc takeWhileState [S; T](state: S; item: Optional[T]): TakeWhileState[S, T] =
  TakeWhileState[S, T](state: state, item: item)



func stream* [S; T](
  initialState: () -> S;
  hasMore: S -> bool;
  generate: S -> T;
  nextStep: S -> S
): Stream[S, T] =
  Stream[S, T](
    initialState: initialState,
    hasMore: hasMore,
    generate: generate,
    nextStep: nextStep
  )



func emptyStream* (T: typedesc): Stream[Unit, T] =
  stream[Unit, T](() => unit(), (_: Unit) => false, nil, itself)


func emptyStream* [T](): FilteredStream[Unit, T] =
  T.emptyStream()


func singleElementStream* [T](element: () -> T): Stream[SingleElementState, T] =
  stream(
    () => singleElementState(true),
    s => s.hasMore,
    _ => element(),
    s => singleElementState(not s.hasMore)
  )



func infiniteStream* [T](generator: () -> T): Stream[Unit, T] =
  stream(() => unit(), _ => true, _ => generator(), itself)


func infiniteStream* [T](initialItem: () -> T; f: T -> T): Stream[T, T] =
  stream(initialItem, _ => true, itself, f)



func map* [S; A; B](self: Stream[S, A]; f: A -> B): Stream[S, B] =
  stream(
    self.initialState,
    self.hasMore,
    state => self.generate(state).f(),
    self.nextStep
  )


func map* [S; A; B](
  self: FilteredStream[S, A];
  f: A -> B
): FilteredStream[S, B] =
  self.map((opt: Optional[A]) => opt.map(f))


func map* [S1; S2; A; B](
  self: FlatStream[S1, S2, A];
  f: A -> B
): FlatStream[S1, S2, B] =
  self.map((s: Stream[S2, A]) => s.map(f))



func flatMap* [SA; A; SB; B](
  self: Stream[SA, A];
  f: A -> Stream[SB, B]
): FlatStream[SA, SB, B] =
  self.map(f)


func flatMap* [SA; A; SB; B](
  self: FilteredStream[SA, A];
  f: A -> Stream[SB, B]
): FlatStream[SA, SB, B] =
  self.map(f)



proc applyPredicate [T](item: T; predicate: T -> bool): Optional[T] =
  item.predicate().ifElse(() => item.toSome(), toNone)



func filter* [S; T](
  self: Stream[S, T];
  predicate: T -> bool
): FilteredStream[S, T] =
  self.map(item => item.applyPredicate(predicate))


func filter* [S; T](
  self: FilteredStream[S, T];
  predicate: T -> bool
): FilteredStream[S, T] =
  self.map((opt: Optional[T]) => opt.filter(predicate))


func filter* [S1; S2; T](
  self: FlatStream[S1, S2, T];
  predicate: T -> bool
): FlatStream[S1, S2, Optional[T]] =
  self.map((s: Stream[S2, T]) => s.filter(predicate))



func limit* [S; T; U](self: Stream[S, T]; n: U): Stream[LimitState[S, U], T] =
  stream(
    () => limitState(self.initialState(), 0.U),
    limit => limit.i < n and self.hasMore(limit.state),
    limit => self.generate(limit.state),
    (limit: LimitState[S, U]) =>
      limitState(self.nextStep(limit.state), limit.i + 1)
  )



proc generateTakeWhileState [S; T](
  state: S;
  hasMore: S -> bool;
  generate: S -> T;
  predicate: T -> bool
): TakeWhileState[S, T] =
  takeWhileState(
    state,
    state.hasMore().ifElse(
      () => state.generate().applyPredicate(predicate),
      toNone
    )
  )


func takeWhile* [S; T](
  self: Stream[S, T];
  predicate: T -> bool
): Stream[TakeWhileState[S, T], T] =
  stream(
    () =>
      self.initialState().generateTakeWhileState(
        self.hasMore, self.generate, predicate
      )
    ,
    twState => twState.item.isSome(),
    twState => twState.item.get(),
    (twState: TakeWhileState[S, T]) =>
      self.nextStep(twState.state).generateTakeWhileState(
        self.hasMore, self.generate, predicate
      )
  )


func takeWhile* [S1; S2; T](
  self: FlatStream[S1, S2, T];
  predicate: T -> bool
): FlatStream[S1, S2, T] =
  self.map(s => s.takeWhile(predicate))



proc reduce* [S; A; B](
  self: Stream[S, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): B =
  var state = self.initialState()
  result = initialResult()

  while self.hasMore(state):
    result = result.accumulate(self.generate(state))
    state = self.nextStep(state)


proc reduce* [S; A; B](
  self: FilteredStream[S, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): B =
  self.reduce(
    initialResult,
    (acc: B, opt: Optional[A]) =>
      opt.ifSome(item => acc.accumulate(item), () => acc)
  )


proc reduce* [S1; S2; A; B](
  self: FlatStream[S1, S2, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): B =
  self
    .map(s => s.reduce(initialResult, accumulate))
    .reduce(initialResult, accumulate)



proc reduceIfNotEmpty* [S; A; B](
  self: Stream[S, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): Optional[B] =
  self.initialState().apply(
    (initialState: S) =>
      self.hasMore(initialState).ifElse(
        () =>
          stream(
            () => self.nextStep(initialState),
            self.hasMore,
            self.generate,
            self.nextStep
          ).reduce(
            initialResult().accumulate(self.generate(initialState)),
            accumulate
          ).toSome()
        ,
        toNone[B]
      )
  )


proc reduceIfNotEmpty* [S; A; B](
  self: FilteredStream[S, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): Optional[B] =
  self.reduceIfNotEmpty(
    initialResult,
    (acc: B, opt: Optional[A]) =>
      opt.ifSome(item => acc.accumulate(item), () => acc)
  )


proc reduceIfNotEmpty* [S1; S2; A; B](
  self: FlatStream[S1, S2, A];
  initialResult: () -> B;
  accumulate: (accumulation: B, item: A) -> B
): Optional[B] =
  self
    .map(s => s.reduceIfNotEmpty(initialResult, accumulate))
    .reduce(initialResult, accumulate)



proc forEach* [S; T](self: Stream[S, T]; consume: T -> Unit): Unit =
  self.reduce(() => unit(), (_, item) => item.consume())


proc forEach* [S; T](self: FilteredStream[S, T]; consume: T -> Unit): Unit =
  self.reduce(() => unit(), (_, item: T) => item.consume())


proc forEach* [S1; S2; T](
  self: FlatStream[S1, S2, T];
  consume: T -> Unit
): Unit =
  self.reduce(() => unit(), (_, item: T) => item.consume())




proc sum* [S; T: SomeNumber](self: Stream[S, T]; R: typedesc[SomeNumber]): R =
  self.reduce(() => 0.R, (acc, item) => acc + item.R)


proc sum* [S; T: SomeNumber](
  self: FilteredStream[S, T];
  R: typedesc[SomeNumber]
): R =
  self.reduce(() => 0.R, (acc: R, item: T) => acc + item.R)


proc sum* [S1; S2; T: SomeNumber](
  self: FlatStream[S1, S2, T];
  R: typedesc[SomeNumber]
): R =
  self.reduce(() => 0.R, (acc: R, item: T) => acc + item.R)



proc sum* [S; T: SomeNumber; R: SomeNumber](self: Stream[S, T]): R =
  self.sum(R)


proc sum* [S; T: SomeNumber; R: SomeNumber](self: FilteredStream[S, T]): R =
  self.sum(R)


proc sum* [S1; S2; T: SomeNumber; R: SomeNumber](
  self: FlatStream[S1, S2, T]
): R =
  self.sum(R)



proc count* [S; T](self: Stream[S, T]; R: typedesc[SomeUnsignedInt]): R =
  self.map(_ => 1.R).sum(R)


proc count* [S; T](
  self: FilteredStream[S, T];
  R: typedesc[SomeUnsignedInt]
): R =
  self.map((_: T) => 1.R).sum(R)


proc count* [S1; S2; T](
  self: FlatStream[S1, S2, T];
  R: typedesc[SomeUnsignedInt]
): R =
  self.map(count[S2, T, R]).sum(R)



proc count* [S; T; R](self: Stream[S, T]): R =
  self.count(R)


proc count* [S; T; R](self: FilteredStream[S, T]): R =
  self.count(R)


proc count* [S1; S2; T; R](self: FlatStream[S1, S2, T]): R =
  self.count(R)



proc findFirst* [S; T](self: Stream[S, T]): Optional[T] =
  self.initialState().apply(
    initial =>
      self.hasMore(initial).ifElse(
        () => self.generate(initial).toSome(),
        toNone
      )
  )


proc findFirst* [S; T](self: FilteredStream[S, T]): Optional[T] =
  self.findFirst().flatMap(opt => opt)


proc findFirst* [S1; S2; T](self: FlatStream[S1, S2, T]): Optional[T] =
  self.map(s => s.findFirst()).findFirst()



when isMainModule:
  import std/[os, sequtils, strutils, unittest]



  func indexes [T](s: seq[T]): Stream[Natural, Natural] =
    stream(() => s.low().Natural, i => i < s.len(), i => i, i => i.succ())


  func items [T](s: seq[T]): Stream[Natural, T] =
    s.indexes().map(i => s[i])


  func chars (s: string): Stream[Natural, char] =
    stream(() => s.low().Natural, i => i < s.len(), i => s[i], i => i.succ())


  func items [T: Ordinal](slice: Slice[T]): Stream[int64, T] =
    stream(
      () => slice.a.int64,
      elm => elm <= slice.b.int64,
      elm => elm.T,
      elm => elm + 1
    )


  func items [T: Ordinal](s: set[T]): FilteredStream[int64, T] =
    items(T.low() .. T.high()).filter(it => it in s)



  func pairs [T](
    s: seq[T]
  ): Stream[Natural, tuple[item: T; index: Natural]] =
    s.indexes().map(i => (s[i], i))



  suite currentSourcePath().splitFile().name:
    test "stream: seq":
      proc streamTest (source: seq[string]) =
        let sut =
          source
          .pairs()
          .filter(pair => pair.item.len() > 3)
          .map(
            (pair: tuple[item: string; index: Natural]) =>
              (item: pair.item, length: pair.item.len(), index: pair.index)
          )

        sut
          .forEach(
            proc (
              elm: tuple[item: string; length: Natural; index: Natural]
            ): Unit =
              let (item, length, index) = elm

              check:
                length > 3
                item == source[index]
                length == source[index].len()
          ).ignore()


      streamTest(@["abc", "a", "abcde", currentSourcePath()])



    test "reduce: slice -> set":
      proc reduceTest (source: Slice[char]) =
        require:
          source.len() > 0

        let sut =
          source
          .items()
          .reduce(() => {}.set[:char], (acc, item) => acc + {item})

        sut
          .items()
          .forEach(
            proc (it: char): Unit =
              check:
                it in source
          ).ignore()


      reduceTest('0'..'9')



    test "findFirst: compile time":
      proc findFirstTest [T](source: static[seq[T]]) =
        require:
          source.len() > 0

        const sut = source.pairs().findFirst()

        check:
          sut.isSome()

        sut
          .get()
          .apply(
            proc (pair: tuple[item: T; index: Natural]): Unit =
              check:
                pair.index == source.low()
                pair.item == source[source.low()]
          ).ignore()


      findFirstTest(@[(0, 1), (1, 0)])



    test "count: filtered":
      proc countTest (U: typedesc[SomeUnsignedInt]) =
        let
          source = 0 .. 100
          validRange = 15 .. 80

        require:
          source.len() > 0
          validRange.a in source
          validRange.b in source

        let
          expected = validRange.len().U
          sut = source.items().filter(i => i in validRange).count(U)

        check:
          sut == expected


      countTest(uint)



    test "count: compile time":
      proc countTest [T](source: seq[T]; U: typedesc[SomeUnsignedInt]) =
        let
          expected = source.len().U
          sut = source.items().count(U)

        check:
          sut == expected


      countTest(@[-5, 16, 95465, 0, -3547], uint64)



    test "emptyStream":
      proc emptyStreamTest (T: typedesc) =
        let
          expected = 0u
          sut = T.emptyStream().count(expected.typeof())

        check:
          sut == expected


      emptyStreamTest((char, uint))



    test "singleElementStream":
      proc singleElementStreamTest [T](element: () -> T) =
        let
          expected = 1u
          sut = singleElementStream(element).count(expected.typeof())

        check:
          sut == expected


      singleElementStreamTest(() => 2)
      singleElementStreamTest(() => 'a')
      singleElementStreamTest(() => newException(RangeError, ""))



    test "infiniteStream: generator, limit":
      proc infiniteStreamTest [T; U: SomeUnsignedInt](
        generator: () -> T;
        expectedLimit: U
      ) =
        let sut = infiniteStream(generator).limit(expectedLimit).count(U)

        check:
          sut == expectedLimit


      infiniteStreamTest(() => (5, '"'), 0.uint)
      infiniteStreamTest(() => "abc", 13.uint)



    test "infiniteStream: seed, limit":
      proc infiniteStreamTest [T; U: SomeUnsignedInt](
        seed: () -> T;
        f: T -> T;
        expectedLimit: U
      ) =
        let sut = infiniteStream(seed, f).limit(expectedLimit).count(U)

        check:
          sut == expectedLimit



      infiniteStreamTest(() => 0u32, i => i + 1, 100.uint64)



    test "takeWhile: count ASCII chars":
      proc takeWhileTest (U: typedesc[SomeUnsignedInt]) =
        let
          source = "abc65abc"
          expected = 3.U
          sut = source.chars().takeWhile(c => c.isAlphaAscii()).count(U)

        check:
          sut == expected


      takeWhileTest(uint64)



    test "takeWhile: count ASCII spaces, compile time":
      proc takeWhileTest [U: SomeUnsignedInt]() =
        const
          source = " \t\n, abc"
          expected = 3.U
          sut = source.chars().takeWhile(c => c.isSpaceAscii()).count(U)

        check:
          sut == expected


      takeWhileTest[uint32]()



    test "FlatStream: seq[string], count characters":
      proc flatStreamTest (source: seq[string]; U: typedesc[SomeUnsignedInt]) =
        let
          expected = source.foldl(a + b.len().U, 0.U)
          sut = source.items().flatMap(chars).count(U)

        check:
          sut == expected


      flatStreamTest(@[], uint)
      flatStreamTest(@["abc"], uint)
      flatStreamTest(@["", "01 ab", "abc"], uint)



    test "FlatStream: seq[string], count letters":
      proc flatStreamTest (source: seq[string]; U: typedesc[SomeUnsignedInt]) =
        let
          expected =
            source.foldl(a + b.filterIt(it.isAlphaAscii()).len().U, 0.U)
          sut =
            source
            .items()
            .flatMap(chars)
            .filter(isAlphaAscii)
            .map((s: FilteredStream[Natural, char]) => s.count(U))
            .sum(U)

        check:
          sut == expected


      flatStreamTest(@[], uint)
      flatStreamTest(@["abc01abc, az"], uint)
      flatStreamTest(@["", "01 ab", "abc"], uint)
