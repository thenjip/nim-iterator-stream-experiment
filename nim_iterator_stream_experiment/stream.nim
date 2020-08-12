##[
  A stream behaves like an iterator. It generates items in a loop and the user
  does something with them.

  A stream is meant to be consumed once, when a final operation is executed.
  Therefore, a stream must release all the resources it held when closing
  (file handles, non GC-ed memory, etc.).

  Any exception raised while generating items in a stream will be re-raised
  after closing the stream.

  Types of operations
  ===================

  Final
  -----

  An operation is final if it does not return a stream or a procedure that
  returns one.

  Intermediate
  ------------

  Operations that return a stream or a lambda that will are called intermediate.
  They can be stateless or stateful.

  Stateless
  ~~~~~~~~~

  Intermediate stateless operations always return a stream without changing the
  step type (`S`).

  Examples: `map`, `filter`.

  Stateful
  ~~~~~~~~

  Intermediate stateful operations can return:
    - A stream with a different step type.

      Examples: `takeWhile`, `limit`

    - A lambda that returns a stream.

      Examples: `dropWhile`, `skip`

      This is to prevent the parallelization of this kind of operation (API to
      be done).
]##



import monad/[identity, io, optional, predicate, reader]
import optics/[lens]
import stream/[loop, streamsteps]
import stream/loop/[loopscope]
import stream/loop/loopscope/[runonceresult]
import
  utils/[
    convert,
    ifelse,
    lambda,
    operators,
    partialprocs,
    reducer,
    somenatural,
    unit,
    variables
  ]

import std/[sugar]



type
  Initializer* [T] = IO[T]
  OnCloseEvent* [T] = Reader[T, Unit]

  Stream* [S; T] = object
    initialStep: Initializer[S]
    loop: Loop[S, T]
    onCloseEvent: OnCloseEvent[S]



func initializer* [T](self: IO[T]): Initializer[T] =
  self


func onCloseEvent* [T](self: Reader[T, Unit]): OnCloseEvent[T] =
  self



template stepType* [S; T](X: typedesc[Stream[S, T]]): typedesc[S] =
  S


template stepType* [S; T](self: Stream[S, T]): typedesc[S] =
  self.typeof().stepType()



template itemType* [S; T](X: typedesc[Stream[S, T]]): typedesc[T] =
  T


template itemType* [S; T](self: Stream[S, T]): typedesc[T] =
  self.typeof().itemType()



func startingAt* [S; T](
  loop: Loop[S, T];
  initialStep: Initializer[S];
  onCloseEvent: OnCloseEvent[S]
): Stream[S, T] =
  Stream[S, T](initialStep: initialStep, loop: loop, onCloseEvent: onCloseEvent)


func startingAt* [S; T](
  loop: Loop[S, T];
  initialStep: Initializer[S]
): Stream[S, T] =
  ## An overload that will do nothing when the returned stream is closed.
  loop.startingAt(initialStep, doNothing[S])



func initialStep* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Initializer[S]] =
  lens(
    (self: X) => self.initialStep,
    (self: X, initialStep: Initializer[S]) =>
      self.loop.startingAt(initialStep, self.onCloseEvent)
  )


func onCloseEvent* [S; T](X: typedesc[Stream[S, T]]): Lens[X, OnCloseEvent[S]] =
  lens(
    (self: X) => self.onCloseEvent,
    (self: X, event: OnCloseEvent[S]) =>
      self.loop.startingAt(self.initialStep, event)
  )


func loop* [S; A](
  X: typedesc[Stream[S, A]];
  B: typedesc
): PLens[X, Loop[S, A], Loop[S, B], Stream[S, B]] =
  lens(
    (self: X) => self.loop,
    (self: X, loop: Loop[S, B]) =>
      loop.startingAt(self.initialStep, self.onCloseEvent)
  )


func loop* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Loop[S, T]] =
  X.loop(T)


func scope* [S; T](X: typedesc[Stream[S, T]]): Lens[X, LoopScope[S]] =
  X.loop().chain(Loop[S, T].scope())


func condition* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Condition[S]] =
  X.loop().chain(Loop[S, T].condition())


func generator* [S; A](
  X: typedesc[Stream[S, A]];
  B: typedesc
): PLens[X, Generator[S, A], Generator[S, B], Stream[S, B]] =
  X.loop(B).chain(Loop[S, A].generator(B))


func generator* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Generator[S, T]] =
  X.generator(T)


func stepper* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Stepper[S]] =
  X.loop().chain(Loop[S, T].stepper())



proc run* [S](self: Stream[S, Unit]): S =
  self
    .initialStep
    .tryBracket(partial(self.loop.run(?_)), self.onCloseEvent)
    .run()



proc mapSteps* [SA; SB; T](
  self: Stream[SA, T];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB];
  initialStepMapper: SA -> SB
): Stream[SB, T] =
  proc buildResult (
    self: self.typeof();
    extractor: extractor.typeof();
    stepperMapper: stepperMapper.typeof();
    initialStepMapper: initialStepMapper.typeof()
  ): result.typeof() =
    self
      .loop
      .mapSteps(extractor, stepperMapper)
      .startingAt(
        self.initialStep.map(initialStepMapper),
        extractor.map(self.onCloseEvent)
      )

  self.buildResult(extractor, stepperMapper, initialStepMapper)



func emptyStream* (T: typedesc): Stream[EmptyStep, T] =
  emptyLoop(EmptyStep, T).startingAt(emptyStep)


func singleItemStream* [T](item: () -> T): Stream[SingleStep, T] =
  func buildResult (
    item: item.typeof();
    S: typedesc[SingleStep]
  ): result.typeof() =
    S
      .isConsumed()
      .read()
      .looped(alwaysFalse[S].map(singleStep))
      .generating((_: S) => item())
      .startingAt(() => singleStep(true))

  item.buildResult(SingleStep)



func onClose* [S; T](self: Stream[S, T]; callBack: () -> Unit): Stream[S, T] =
  self.modify(
    self.typeof().onCloseEvent(),
    partial(map(?_, _ => callBack.run()))
  )



func map* [S; A; B](self: Stream[S, A]; f: A -> B): Stream[S, B] =
  self.modify(self.typeof().generator(B), partial(map(?_, f)))


func filter* [S; T](self: Stream[S, T]; predicate: Predicate[T]): Stream[S, T] =
  let dropSteps = self.loop.dropWhile(not predicate)

  self
    .modify(self.typeof().initialStep(), partial(map(?_, dropSteps)))
    .modify(self.typeof().stepper(), partial(map(?_, dropSteps)))



func peek* [S; T](self: Stream[S, T]; f: T -> Unit): Stream[S, T] =
  self.map(it => f.run(it).apply(_ => it))



func limit* [S; T; N: SomeNatural](
  self: Stream[S, T];
  n: N
): Stream[LimitStep[S, N], T] =
  let limitLenses =
    (step: LimitStep[S, N].step(), count: LimitStep[S, N].count())

  self
    .mapSteps(
      limitLenses.step.read(),
      stepper =>
        limitLenses.step.modify(stepper).map(limitLenses.count.modify(next[N]))
      ,
      partial(limitStep(?:S, 0 as N))
    ).modify(
      result.typeof().scope(),
      scope => scope.breakIf(limitLenses.count.read().map(partial(?_ >= n)))
    )


func skip* [S; T; N: SomeNatural](self: Stream[S, T]; n: N): IO[Stream[S, T]] =
  let
    skLenses =
      (step: SkipStep[S, N].step(), count: SkipStep[S, N].count())
    readSkStep = skLenses.step.read()
    skipSteps =
      self
        .read(self.typeof().scope())
        .mapSteps(
          (hasMore: Condition[S]) =>
            readSkStep
              .map(hasMore)
              .`and`(skLenses.count.read().map(partial(?_ < n)))
          ,
          (stepper: Stepper[S]) =>
            skLenses.step.modify(stepper).map(skLenses.count.modify(next))
        ).asReader()

  self
    .modify(
      self.typeof().initialStep(),
      (start: Initializer[S]) =>
        start.map(partial(skipStep(?::S, 0.N))).map(skipSteps).map(readSkStep)
    ).lambda()



func runOnceToTakeWhileStep [S; T](
  runOnce: RunOnceResult[S, T]
): TakeWhileStep[S, T] =
  takeWhileStep(
    runOnce.read(runOnce.typeof().step()),
    runOnce.read(runOnce.typeof().item())
  )


func takeWhile [S; T](
  loop: Loop[S, T];
  predicate: Predicate[T]
): Loop[TakeWhileStep[S, T], T] =
  let
    itemLens = TakeWhileStep[S, T].item()
    readItem = itemLens.read()
    readStep = TakeWhileStep[S, T].step().read()

  loop
    .mapSteps(
      readStep,
      (_: Stepper[S]) =>
        readStep
          .map(partial(loop.runOnce(?_)))
          .map(runOnceToTakeWhileStep)
          .map(itemLens.modify(partial(filter(?:Optional[T], predicate))))
    ).write(result.typeof().condition(), readItem.map(isSome))
    .write(result.typeof().generator(), readItem.map(unbox))


func takeWhile* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): Stream[TakeWhileStep[S, T], T] =
  let twLoop = self.loop.takeWhile(predicate)

  twLoop.startingAt(
    self
      .initialStep
      .map(partial(takeWhileStep(?:S, T.toNone())))
      .map(twLoop.read(twLoop.typeof().stepper()))
    ,
    twLoop.typeof().S.step().read().map(self.onCloseEvent)
  )


func dropWhile* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): IO[Stream[S, T]] =
  self
    .modify(
      self.typeof().initialStep(),
      partial(map(?:Initializer[S], self.loop.dropWhile(predicate)))
    ).lambda()



proc forEach* [S; T](self: Stream[S, T]; action: T -> Unit): Unit =
  self.map(action).run().doNothing()



proc reduce* [S; T; R](
  self: Stream[S, T];
  reducer: Reducer[R, T];
  initialResult: R
): R =
  var reduction = initialResult

  self
    .forEach(
      item => reduction.modify(partial(reducer.run(?_, item))).doNothing()
    ).apply(_ => reduction.read())


proc reduceIfNotEmpty* [S; T; R](
  self: Stream[S, T];
  reducer: Reducer[R, T];
  initialResult: R
): Optional[R] =
  let reduceOnNextStep =
    (step: S) =>
      self
        .write(
          self.typeof().initialStep(),
          () => self.read(self.typeof().stepper()).run(step)
        ).reduce(
          reducer,
          reducer
            .run(initialResult, self.read(self.typeof().generator()).run(step))
        )

  self
    .initialStep
    .map(
      self
        .read(self.typeof().condition())
        .ifElse(reduceOnNextStep.map(toSome), _ => R.toNone())
    ).run()



proc sum* [S; N](self: Stream[S, N]): N =
  ## `+` must be defined for `N`.
  self.reduce(plus[N], 0 as N)


proc count* [S; T](self: Stream[S, T]; N: typedesc[SomeNatural]): N =
  self.map((_: T) => 1.N).sum()


proc count* [S; T; N: SomeNatural](self: Stream[S, T]): N =
  self.count(N)



proc findFirst* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): Optional[T] =
  self
    .dropWhile(not predicate)
    .flatMap(
      (self: self.typeof()) =>
        self.initialStep.tryBracket(
          partial(self.loop.runOnce(?:S)),
          self.onCloseEvent
        )
    ).map(RunOnceResult[S, T].item().read())
    .run()


proc findFirst* [S; T](self: Stream[S, T]): Optional[T] =
  self.findFirst(alwaysTrue[T])



proc any* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  self.findFirst(predicate).isSome()


proc all* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  not self.any(not predicate)


proc none* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  self.all(not predicate)



when isMainModule:
  import optics/[lenslaws]
  import utils/[ignore]

  import std/[os, strutils, unittest]



  type
    InfiniteStreamParam [S; T] = tuple
      stepper: Stepper[S]
      generator: Generator[S, T]
      initialStep: Initializer[S]
      onClose: OnCloseEvent[S]



  func infiniteStreamParam [S; T](
    stepper: Stepper[S];
    generator: Generator[S, T];
    initialStep: Initializer[S];
    onClose: OnCloseEvent[S]
  ): InfiniteStreamParam[S, T] =
    (stepper, generator, initialStep, onClose)



  func items (s: Slice[Natural]): Stream[Natural, Natural] =
    partial(?:result.stepType() <= s.b)
      .looped(next)
      .generating(itself[result.stepType()])
      .startingAt(() => s.a)


  func items [T](s: seq[T]): Stream[Natural, T] =
    items(s.low().Natural .. s.high().Natural).map(i => s[i])


  func addToSeq [T](s: seq[T]; item: T): seq[T] =
    s & item



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"Stream[S, T].initialStep()" should return a lens that verifies the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[Stream[S, T], Initializer[S]]) =
          check:
            Stream[S, T].initialStep().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(singleItemStream(() => 1)),
            retentionSpec(
              singleItemStream(() => -1),
              nil as Initializer[SingleStep]
            ),
            doubleWriteSpec(
              singleItemStream(() => int.low()),
              initializer(() => singleStep(false)),
              () => singleStep(true)
            )
          )
        )



      test """"Stream[S, T].onCloseEvent()" should return a lens that verifies the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[Stream[S, T], OnCloseEvent[S]]) =
          check:
            Stream[S, T].onCloseEvent().checkLensLaws(spec)


        doTest(
          lensLawsSpec(
            identitySpec(emptyStream(string)),
            retentionSpec(
              emptyStream(string),
              onCloseEvent(
                proc (_: EmptyStep): Unit =
                  echo("a")
              )
            ),
            doubleWriteSpec(
              emptyStream(string),
              onCloseEvent(doNothing[EmptyStep]),
              proc (_: EmptyStep): Unit =
                debugEcho("0", 1)
            )
          )
        )



      test """"Stream[S, T].loop()" should return a lens that verifies the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[Stream[S, T], Loop[S, T]]) =
          check:
            Stream[S, T].loop().checkLensLaws(spec)


        proc runTest1 () =
          let
            stream =
              next[Natural]
                .infiniteLoop(partial($ ?:Natural))
                .startingAt(() => Natural.low())
            streamGenerator = stream.read(stream.typeof().generator())


          doTest(
            lensLawsSpec(
              identitySpec(stream),
              retentionSpec(stream, emptyLoop(stream.stepType(), string)),
              doubleWriteSpec(
                stream,
                partial(?:stream.stepType() < 10)
                  .looped(next)
                  .generating(streamGenerator),
                partial(?:stream.stepType() > Natural.low())
                  .looped(prev)
                  .generating(streamGenerator)
              )
            )
          )


        runTest1()



      test """Raising an exception in a stream's loop should close the stream and raise that exception again.""":
        proc doTest () =
          var streamClosed = false

          let stream =
            partial(?:int < 10)
              .looped(next)
              .generating(
                partial(?:int == 4).ifElse(
                  proc (_: auto): int = raise ValueError.newException(""),
                  itself
                )
              ).startingAt(
                () => 0,
                proc (_: int): Unit = streamClosed = true
              )

          expect Exception:
            stream.sum().ignore()

          check:
            streamClosed


        doTest()



      test """Using "reduceIfNotEmpty" on an empty stream should return an empty "Optional[R]".""":
        proc doTest [T; R](reducer: Reducer[R, T]; initialResult: R) =
          let
            actual = emptyStream(T).reduceIfNotEmpty(reducer, initialResult)
            expected = R.toNone()

          check:
            actual == expected


        doTest(plus[int], 0)
        doTest(mult[cdouble], 100.0)
        doTest(partial(?:string & ?:string), "abc")



      test """Counting the number of items in a stream of 1 item should return 1.""":
        proc doTest [T](item: () -> T) =
          let
            actual = singleItemStream(item).count(Natural)
            expected = 1 as actual.typeof()

          check:
            actual == expected


        doTest(() => ValueError.newException(""))
        doTest(() => 'a')



      test """Limiting an infinite stream to "n" items and collecting them in a "seq[T]" should return a sequence of "n" items.""":
        proc doTest [S; T; N](param: InfiniteStreamParam[S, T]; n: N) =
          let
            actual =
              param
                .stepper
                .infiniteLoop(param.generator)
                .startingAt(param.initialStep, param.onClose)
                .limit(n)
                .reduce(addToSeq[T], @[])
                .len()
                .convert(N)
            expected = n

          check:
            actual == expected


        doTest(
          infiniteStreamParam(
            stepper(itself[uint16]),
            when defined(js):
              generator((i: uint16) => (i, i))
            else:
              generator(partial($ ?:uint16))
            ,
            () => 1u16,
            doNothing[uint16]
          ),
          10u
        )



      test """Skipping "ns" items of a infinite stream limited to "nl" items and collecting them in a "seq[T]" should return a sequence of "ns" items.""":
        proc doTest [S; T; N](param: InfiniteStreamParam[S, T]; nl, ns: N) =
          require:
            ns <= nl

          let
            actual =
              param
                .stepper
                .infiniteLoop(param.generator)
                .startingAt(param.initialStep, param.onClose)
                .limit(nl)
                .skip(ns)
                .run()
                .reduce(addToSeq[T], @[])
                .len()
                .convert(N)
            expected = nl - ns

          check:
            actual == expected


        doTest(
          infiniteStreamParam(
            stepper(next[Natural]),
            generator(itself[Natural]),
            () => 0 as Natural,
            doNothing[Natural]
          ),
          20u,
          5u
        )



      test(
        """Taking the items of a stream of "Positive"s, starting at "Positive.low()",""" &
          """ while the current item is less than 10 and collecting them at compile time should return "@[Positive.low() .. 9]"."""
      ):
        func items (P: typedesc[Positive]): Stream[P, P] =
          partial(?:P < P.high())
            .looped(next)
            .generating(itself[P])
            .startingAt(() => P.low())


        func takeWhileAndCollect (P: typedesc[Positive]): seq[Positive] =
          result = newSeqOfCap[P](9)

          for it in P.low() ..< P.high():
            if it < 10:
              result.add(it)
            else:
              break


        proc doTest () =
          const
            actual =
              Positive
                .items()
                .takeWhile(partial(?:Positive < 10))
                .reduce(addToSeq[Positive], newSeqOfCap[Positive](9))
            expected = Positive.takeWhileAndCollect()

          check:
            actual == expected


        doTest()



      test """"self.findFirst(predicate)" should return the first item that verifies "predicate".""":
        proc doTest [S; T](
          self: Stream[S, T];
          predicate: Predicate[T];
          expected: Optional[T]
        ) =
          let actual = self.findFirst(predicate)

          check:
            actual == expected


        proc runTest1 () =
          let expected = 35.Natural

          doTest(
            items(0.convert(expected.typeof()) .. 100.convert(expected.typeof())),
            partial(?:expected.typeof() == expected),
            expected.toSome()
          )


        proc runTest2 () =
          let
            stream = @["", "a", "abc", " \n"].items()
            expected = stream.itemType().toNone()

          doTest(stream, (s: stream.itemType()) => s.len() > 5, expected)


        runTest1()
        runTest2()
        doTest(Natural.emptyStream(), alwaysTrue[Natural], Natural.toNone())



      test """"self.any(predicate)" should check whether any item verifies "predicate".""":
        proc doTest [S; T](
          self: Stream[S, T];
          predicate: Predicate[T];
          expected: bool
        ) =
          let actual = self.any(predicate)

          check:
            actual == expected


        proc runTest1 () =
          let stream = @[-5, -1041, 683, 40, 339].items()

          doTest(stream, (i: stream.itemType()) => i mod 2 == 0, true)


        proc runTest2 () =
          let stream = @[@[0, 1], @[-8, 9, 10]].items()

          doTest(stream, (it: stream.itemType()) => it.len() == 0, false)


        runTest1()
        runTest2()
        doTest(char.emptyStream(), alwaysTrue[char], false)



      test """"self.all(predicate)" should check whether all the items verify "predicate".""":
        proc doTest [S; T](
          self: Stream[S, T];
          predicate: Predicate[T];
          expected: bool
        ) =
          let actual = self.all(predicate)

          check:
            actual == expected


        proc runTest1 () =
          let stream = @[('a', 1), (' ', -5), ('\t', 7), (']', -100)].items()

          doTest(stream, (it: stream.itemType()) => not it[0].isDigit(), true)


        proc runTest2 () =
          let stream = @[new int, nil, new int].items().map(toOptional)

          doTest(stream, isSome[stream.itemType().valueType()], false)


        runTest1()
        runTest2()
        doTest(uint.emptyStream(), alwaysFalse[uint], true)



      test """"self.none(predicate)" should check whether no items verify "predicate".""":
        proc doTest [S; T](
          self: Stream[S, T];
          predicate: Predicate[T];
          expected: bool
        ) =
          let actual = self.none(predicate)

          check:
            actual == expected


        proc runTest1 () =
          let stream = items(132.Natural .. 956.Natural)

          doTest(stream, (it: stream.itemType()) => it mod 1000 == 0, true)


        proc runTest2 () =
          let stream = @[false, true, false, false].items()

          doTest(stream, itself[stream.itemType()], false)


        runTest1()
        runTest2()
        doTest(Unit.emptyStream(), alwaysTrue[Unit], true)



  main()
