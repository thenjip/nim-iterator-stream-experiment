import loop
import loop/[loopscope, loopsteps]
import monad/[identity, io, optional, predicate, reader]
import optics/[focus, lens]
import stream/[streamsteps]
import
  utils/[
    convert,
    ignore,
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



proc hasMore [S; T](self: Stream[S, T]; step: S): bool =
  self.read(self.typeof().condition()).test(step)


proc generate [S; T](self: Stream[S, T]; step: S): T =
  self.read(self.typeof().generator()).run(step)


proc nextStep [S; T](self: Stream[S, T]; step: S): S =
  self.read(self.typeof().stepper()).run(step)


proc run* [S](self: Stream[S, Unit]): Unit =
  self.loop.run(self.initialStep.run()).apply(self.onCloseEvent)



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



func emptyStream* (T: typedesc): Stream[ZeroStep, T] =
  T.emptyLoop().startingAt(zeroStep)


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


func filter* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): Stream[S, Optional[T]] =
  self.map(item => predicate.test(item).ifElse(() => item.toSome(), toNone[T]))


func limit* [S; T; N](self: Stream[S, T]; n: N): Stream[LimitStep[S, N], T] =
  func buildResult [S; N](
    self: self.typeof();
    n: N;
    L: typedesc[LimitStep[S, N]]
  ): result.typeof() =
    self
      .mapSteps(
        L.step().read(),
        stepper =>
          L.step().modify(stepper).map(L.count().modify(partial(?_ + 1)))
        ,
        partial(limitStep(?:S, 0.N))
      ).modify(
        result.typeof().condition(),
        partial(?_ and L.count().read().map(partial(?_ < n)))
      )

  self.buildResult(n, LimitStep[S, N])


func skip* [S; T; N: SomeUnsignedInt](
  self: Stream[S, T];
  n: N
): IO[Stream[S, T]] =
  let readStep = SkipStep[S, N].step().read()

  func buildSkipLoopScope [S; N](
    scope: LoopScope[S];
    n: N;
    SK: typedesc[SkipStep[S, N]]
  ): LoopScope[SK] =
    scope.mapSteps(
      (hasMore: Condition[S]) =>
        readStep.map(hasMore) and SK.count().read().map(partial(?_ < n))
      ,
      (stepper: Stepper[S]) =>
        SK.step().modify(stepper).map(SK.count().modify(partial(?_ + 1)))
    )

  self
    .modify(
      self.typeof().initialStep(),
      (start: Initializer[S]) =>
        start
          .map(partial(skipStep(?::S, 0.N)))
          .map(
            self
              .read(self.typeof().scope())
              .buildSkipLoopScope(n, SkipStep[S, N])
              .asReader()
          ).map(readStep)
    ).lambda()



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
      partial(map(?:Initializer[S], self.loop.dropWhile(predicate).map(toIO)))
        .map(run)
    ).lambda()



proc reduce* [S; T; R](
  self: Stream[S, T];
  reducer: Reducer[R, T];
  initialResult: R
): R =
  var reduction = initialResult

  self
    .map(item => reduction.modify(partial(reducer.run(?_, item))).doNothing())
    .run()
    .ignore()


proc reduceIfNotEmpty* [S; T; R](
  self: Stream[S, T];
  reducer: Reducer[R, T];
  initialResult: R
): Optional[R] =
  let reduceOnNextStep =
    (step: S) =>
      self
        .write(self.typeof().initialStep(), () => self.nextStep(step))
        .reduce(reducer, reducer.run(initialResult, self.generate(step)))

  self
    .initialStep
    .map(
      (initial: S) =>
        self.hasMore(initial).ifElse(
          () => initial.reduceOnNextStep().toSome(),
          toNone[R]
        )
    ).run()


proc forEach* [S; T](self: Stream[S, T]; action: T -> Unit): Unit =
  self.map(action).reduce((u: Unit, _: Unit) => u, unit())



proc sum* [S; N](self: Stream[S, N]): N =
  self.reduce(plus[N], 0.N)


proc count* [S; T](self: Stream[S, T]; N: typedesc): N =
  self.map((_: T) => 1.N).sum()


proc count* [S; T; N](self: Stream[S, T]): N =
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
  import utils/[call, nimnodes]

  import std/[macros, os, unittest]



  suite currentSourcePath().splitFile().name:
    func indexes [I, T](a: array[I, T]): Stream[Natural, Natural] =
        partial(?:Natural < a.len())
          .looped(partial(?_ + 1))
          .generating(itself[Natural])
          .startingAt(() => 0)

    func items [I, T](a: array[I, T]): Stream[Natural, T] =
      a.indexes().map(i => a[i])



    test "test":
      let someArray = [0, 1, 3, 10]

      check:
        someArray.indexes().count(Natural) == someArray.len()

      someArray.indexes().forEach(proc (i: Natural): Unit = echo(i)).ignore()
      someArray
        .items()
        .filter(alwaysTrue[int])
        .forEach(proc (i: Optional[int]): Unit = echo(i))
        .ignore()

      check:
        singleItemStream(() => 0).count(uint) == 1u
        singleItemStream(() => 0).limit(2u).count(uint) == 1u

      check:
        someArray
          .indexes()
          .onClose(() => unit())
          .reduceIfNotEmpty((count: int, _: Natural) => count + 1, 0)
          .isSome()

      check:
        someArray.items().takeWhile((i: int) => i < 5).count(uint) == 3u

      check:
        someArray.items().dropWhile((i: int) => i <= 1).run().count(uint) == 2u

      check:
        someArray.items().findFirst((i: int) => i == 10).isSome()
        someArray.items().findFirst((i: int) => i > 15).isNone()
        someArray.items().findFirst().get() == someArray[0]
        someArray.items().skip(2u).run().findFirst().get() == someArray[2]

      check:
        someArray.items().any((i: int) => i > 0)
        someArray.items().all((i: int) => i >= 0)
        someArray.items().none((i: int) => i < 0)



    test """"Using "self.count(N)" to count the number of children in a NimNode "n" should return "n.len()".""":
      type
        NimNodeStep = object
          i: Natural


      func i [X: NimNodeStep](): Lens[X, Natural] =
        lens((self: X) => self.i, (_: X, i: Natural) => NimNodeStep(i: i))


      func indexes (n: NimNode): Stream[NimNodeStep, Natural] =
        let
          lens = i[NimNodeStep]()
          readIndex = lens.read()

        readIndex
          .map(partial(?:Natural < n.len()))
          .looped(lens.modify(plus1))
          .generating(readIndex)
          .startingAt(() => NimNodeStep(i: n.low()))


      func children (n: NimNode): Stream[NimNodeStep, NimNode] =
        n.indexes().map(i => n[i])


      template doTest (n: NimNode; N: typedesc): proc () {.nimcall.} =
        (
          proc () =
            const
              actual = n.children().count(N)
              expected = n.len().N

            check:
              actual == expected
        )



      for t in [
        doTest(newEmptyNode(), Natural),
        doTest(newStrLitNode("abc"), cuint),
        doTest(
          quote do:
            let a = 0
            var v = nil.cstring
            v = "a"
            echo(a + 1)
          ,
          uint
        )
      ]:
        t.call()
