import loop
import loop/[loopscope]
import monad/[identity, io, optional, predicate, reader]
import optics/[focus, lens]
import stream/[streamsteps]
import types/[somenatural]
import
  utils/[
    convert,
    ifelse,
    lambda,
    operators,
    partialprocs,
    reducer,
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
  self.initialStep.bracket(partial(self.loop.run(?_)), self.onCloseEvent).run()



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
        self.initialStep.bracket(
          partial(self.loop.runOnce(?:S))
            .map(RunOnceResult[S, T].item().read())
          ,
          self.onCloseEvent
        )
    ).run()


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

  import std/[os, unittest]



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
              .reduce((s: seq[T], item: T) => s & item, @[])
              .len()
              .convert(N)
          expected = n

        check:
          actual == expected


      doTest(
        infiniteStreamParam(
          stepper(itself[uint16]),
          generator(partial($ ?:uint16)),
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
              .reduce((s: seq[T], item: T) => s & item, @[])
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
