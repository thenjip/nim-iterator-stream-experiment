import loop
import loop/[loopscope, loopsteps]
import monad/[identity, io, optional, predicate, reader]
import optics/[focus, lens]
import stream/[streamsteps]
import utils/[convert, ignore, ifelse, unit, variables]

import std/[sugar]



type
  OnCloseEvent* [T] = Reader[T, Unit]

  Stream* [S; T] = object
    initialStep: IO[S]
    loop: Loop[S, T]
    onCloseEvent: OnCloseEvent[S]



func startingAt* [S; T](
  loop: Loop[S, T];
  initialStep: IO[S];
  onCloseEvent: OnCloseEvent[S]
): Stream[S, T] =
  Stream[S, T](initialStep: initialStep, loop: loop, onCloseEvent: onCloseEvent)


func startingAt* [S; T](loop: Loop[S, T]; initialStep: IO[S]): Stream[S, T] =
  loop.startingAt(initialStep, doNothing[S])



func initialStep* [S; T](X: typedesc[Stream[S, T]]): Lens[X, IO[S]] =
  lens(
    (self: X) => self.initialStep,
    (self: X, initialStep: IO[S]) =>
      self.loop.startingAt(initialStep, self.onCloseEvent)
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


func onCloseEvent* [S; T](X: typedesc[Stream[S, T]]): Lens[X, OnCloseEvent[S]] =
  lens(
    (self: X) => self.onCloseEvent,
    (self: X, event: OnCloseEvent[S]) =>
      self.loop.startingAt(self.initialStep, event)
  )


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
  self.loop.run(self.initialStep).apply(self.onCloseEvent)



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
      .startingAt(() => true.singleStep())

  item.buildResult(SingleStep)



func onClose* [S; T](self: Stream[S, T]; callBack: () -> Unit): Stream[S, T] =
  self.modify(
    self.typeof().onCloseEvent(),
    event => event.map(_ => callBack.run())
  )



func map* [S; A; B](self: Stream[S, A]; f: A -> B): Stream[S, B] =
  self.modify(self.typeof().generator(B), gen => gen.map(f))


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
        stepper => L.step().modify(stepper).map(L.count().modify(i => i + 1.N)),
        step => step.limitStep(0.N)
      ).modify(
        result.typeof().condition(),
        hasMore => hasMore and L.count().read().map(count => count < n)
      )

  self.buildResult(n, LimitStep[S, N])


func skip* [S; T; N: SomeUnsignedInt](self: Stream[S, T]; n: N): Stream[S, T] =
  func buildSkipLoopScope [S; N; L: LoopScope[S]](
    selfLoopScope: L;
    n: N;
    SK: typedesc[SkipStep[S, N]]
  ): LoopScope[SK] =
    selfLoopScope
      .mapSteps(
        SK.step().read(),
        (stepper: Stepper[S]) =>
          SK.step().modify(stepper).map(SK.count().modify(i => i + 1.N))
      ).modify(
        result.typeof().condition(),
        hasMore => hasMore and SK.count().read().map(count => count < n)
      )

  func buildResult [N; X: self.typeof()](self: X; n: N; S: typedesc): X =
    let skipSteps =
      (initialStep: self.initialStep.typeof()) =>
        initialStep.map(
          (step: S) =>
            self
              .read(X.loop().chain(Loop[S, T].loopScope()))
              .buildSkipLoopScope(n, SkipStep[S, N])
              .asReader(doNothing)
              .map(SkipStep[S, N].step().read())
              .run(skipStep(step, 0.N))
        )

    self.modify(X.initialStep(), skipSteps)

  self.buildResult(n, S)



func takeWhile* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): Stream[TakeWhileStep[S, T], T] =
  let generateWrappedStep =
    (step: S) =>
      step.takeWhileStep(
        self.hasMore(step).ifElse(
          () => self.generate(step).toSome().filter(predicate),
          toNone[T]
        )
      )

  self
    .loop
    .takeWhile(predicate)
    .startingAt(
      self.initialStep.map(generateWrappedStep),
      TakeWhileStep[S, T].step().read().map(self.onCloseEvent)
    )



func dropWhile* [S; T](
  self: Stream[S, T];
  condition: Predicate[T]
): Stream[S, T] =
  func buildDropLoopScope [S; T; L: LoopScope[S]](
    selfLoopScope: L;
    generator: Generator[S, T];
    condition: condition.typeof()
  ): L =
    selfLoopScope.modify(
      L.condition(),
      hasMore => hasMore and generator.map(condition)
    )

  func buildResult [X: self.typeof()](
    self: X;
    condition: condition.typeof();
    S: typedesc
  ): X =
    self.modify(
      X.initialStep(),
      (initialStep: self.initialStep.typeof()) =>
        initialStep.map(
          self
            .read(X.loop().chain(Loop[S, T].loopScope()))
            .buildDropLoopScope(self.read(X.generator()), condition)
            .asReader()
        )
    )

  self.buildResult(condition, S)



proc reduce* [S; T; R](
  self: Stream[S, T];
  reducer: (reduction: R, item: T) -> R;
  initialResult: () -> R
): R =
  var reduction = initialResult.run()

  self
    .map(item => reduction.modify(r => r.reducer(item)).doNothing())
    .run()
    .apply(_ => reduction.read())


proc reduceIfNotEmpty* [S; T; R](
  self: Stream[S, T];
  reducer: (reduction: R, item: T) -> R;
  initialResult: () -> R
): Optional[R] =
  let reduceOnNextStep =
    (step: S) =>
      self
        .write(self.typeof().initialStep(), () => self.nextStep(step))
        .reduce(reducer, initialResult.map(r => r.reducer(self.generate(step))))

  self
    .initialStep
    .map(
      (initialStep: S) =>
        self
          .hasMore(initialStep)
          .ifElse(() => initialStep.reduceOnNextStep().toSome(), toNone[R])
    ).run()


proc forEach* [S; T](self: Stream[S, T]; action: T -> Unit): Unit =
  self.map(action).reduce((u: Unit, _: Unit) => u, () => unit())



proc sum* [S; N](self: Stream[S, N]): N =
  self.reduce((sum: N, item: N) => convert(sum + item, N), () => 0.N)


proc count* [S; T](self: Stream[S, T]; N: typedesc): N =
  self.map((_: T) => 1.N).sum()


proc count* [S; T; N](self: Stream[S, T]): N =
  self.count(N)



proc findFirst* [S; T](
  self: Stream[S, T];
  predicate: Predicate[T]
): Optional[T] =
  let findItem =
    (step: S) =>
      self
        .hasMore(step)
        .ifElse(() => self.generate(step).toSome(), toNone)
        .apply(found => self.onCloseEvent.map(_ => found).run(step))

  self.dropWhile(not predicate).initialStep.map(findItem).run()


proc findFirst* [S; T](self: Stream[S, T]): Optional[T] =
  self.findFirst(alwaysTrue[T])



proc any* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  self.findFirst(predicate).isSome()


proc all* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  not self.any(not predicate)


proc none* [S; T](self: Stream[S, T]; predicate: Predicate[T]): bool =
  self.all(not predicate)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      func indexes [I, T](a: array[I, T]): Stream[Natural, Natural] =
        looped((i: Natural) => i < a.len(), i => i + 1)
          .generating(itself[Natural])
          .startingAt(() => 0)

      func items [I, T](a: array[I, T]): Stream[Natural, T] =
        a.indexes().map(i => a[i])

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
          .reduceIfNotEmpty((count: int, _: Natural) => count + 1, () => 0)
          .isSome()

      check:
        someArray.items().takeWhile((i: int) => i < 5).count(uint) == 3u

      check:
        someArray.items().dropWhile((i: int) => i <= 1).count(uint) == 2u

      check:
        someArray.items().findFirst((i: int) => i == 10).isSome()
        someArray.items().findFirst((i: int) => i > 15).isNone()
        someArray.items().findFirst().get() == someArray[0]
        someArray.items().skip(2u).findFirst().get() == someArray[2]

      check:
        someArray.items().any((i: int) => i > 0)
        someArray.items().all((i: int) => i >= 0)
        someArray.items().none((i: int) => i < 0)
