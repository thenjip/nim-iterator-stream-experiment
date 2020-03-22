import loop
import loop/[loopedcondition, loopsteps]
import monad/[identity, io, optional, reader]
import optics/[focus, lens]
import stream/[streamsteps]
import utils/[convert, ignore, ifelse, predicate, unit, variables]

import std/[sugar]



type
  OnCloseCallBack* [T] = Reader[T, Unit]

  Stream* [S; T] = object
    initialStep: IO[S]
    loop: Loop[S, T]
    closure: OnCloseCallBack[S]



func startingAt* [S; T](
  loop: Loop[S, T];
  initialStep: IO[S];
  closure: OnCloseCallBack[S]
): Stream[S, T] =
  Stream[S, T](initialStep: initialStep, loop: loop, closure: closure)


func startingAt* [S; T](loop: Loop[S, T]; initialStep: IO[S]): Stream[S, T] =
  loop.startingAt(initialStep, doNothing[S])



func initialStep* [S; T](X: typedesc[Stream[S, T]]): Lens[X, IO[S]] =
  lens(
    (self: X) => self.initialStep,
    (self: X, initialStep: IO[S]) =>
      self.loop.startingAt(initialStep, self.closure)
  )


func loop* [S; A](
  X: typedesc[Stream[S, A]];
  B: typedesc
): PLens[X, Loop[S, A], Loop[S, B], Stream[S, B]] =
  lens(
    (self: X) => self.loop,
    (self: X, loop: Loop[S, B]) =>
      loop.startingAt(self.initialStep, self.closure)
  )


func loop* [S; T](X: typedesc[Stream[S, T]]): Lens[X, Loop[S, T]] =
  X.loop(T)


func closure* [S; T](X: typedesc[Stream[S, T]]): Lens[X, OnCloseCallBack[S]] =
  lens(
    (self: X) => self.closure,
    (self: X, closure: OnCloseCallBack[S]) =>
      self.loop.startingAt(self.initialStep, self.closure)
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
  self.focusOn(self.typeof().condition()).read().test(step)


proc generate [S; T](self: Stream[S, T]; step: S): T =
  self.focusOn(self.typeof().generator()).read().run(step)


proc nextStep [S; T](self: Stream[S, T]; step: S): S =
  self.focusOn(self.typeof().stepper()).read().advance(step)


proc run* [S](self: Stream[S, Unit]): Unit =
  self.loop.run(self.initialStep.run()).apply(self.closure)



func wrapSteps* [SA; SB; T](
  self: Stream[SA, T];
  extractor: SB -> SA;
  stepperBuilder: Stepper[SA] -> Stepper[SB];
  initialStepWrapper: SA -> SB
): Stream[SB, T] =
  func buildResult (
    self: self.typeof();
    extractor: extractor.typeof();
    stepperBuilder: stepperBuilder.typeof();
    initialStepWrapper: initialStepWrapper.typeof()
  ): result.typeof() =
    self
      .loop
      .wrapSteps(extractor, stepperBuilder)
      .startingAt(
        self.initialStep.map(initialStepWrapper),
        extractor.map(self.closure)
      )

  self.buildResult(extractor, stepperBuilder, initialStepWrapper)



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
      .convert(Condition[S])
      .looped(alwaysFalse[S].map(singleStep))
      .generating((_: S) => item())
      .startingAt(() => true.singleStep())

  item.buildResult(SingleStep)



func onClose* [S; T](self: Stream[S, T]; callBack: () -> Unit): Stream[S, T] =
  self
    .focusOn(self.typeof().closure())
    .modify(closure => closure.map(_ => callBack.run()))



func map* [S; A; B](self: Stream[S, A]; f: A -> B): Stream[S, B] =
  self.focusOn(self.typeof().generator(B)).modify(gen => gen.map(f))


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
      .wrapSteps(
        L.step().read(),
        stepper =>
          L.step().modify(stepper).map(L.count().modify(count => count + 1.N))
        ,
        step => step.limitStep(0.N)
      ).focusOn(result.typeof().condition())
      .modify(
        (hasMore: Condition[L]) =>
          hasMore
            .`and`(L.count().read().map(count => count < n) as hasMore.typeof())
      )

  self.buildResult(n, LimitStep[S, N])


func skip* [S; T; N: SomeUnsignedInt](self: Stream[S, T]; n: N): Stream[S, T] =
  # TODO
  discard


func takeWhile* [S; T](
  self: Stream[S, T];
  condition: Predicate[T]
): Stream[TakeWhileStep[S, T], T] =
  let generateTakeWhileStep =
    (step: S) =>
      step.takeWhileStep(
        self.hasMore(step).ifElse(
          () => self.generate(step).toSome().filter(condition),
          toNone[T]
        )
      )

  func buildResult [S; T; X: self.typeof()](
    self: X;
    TW: typedesc[TakeWhileStep[S, T]]
  ): Stream[TW, T] =
    self
      .wrapSteps(
        TW.step().read(),
        stepper =>
          TW
            .step()
            .modify(stepper)
            .map(TW.step().read().map(generateTakeWhileStep))
        ,
        generateTakeWhileStep
      ).focusOn(result.typeof().condition())
      .write(TW.item().read().map(isSome).convert(Condition[TW]))

  self.buildResult(TakeWhileStep[S, T])



func dropWhile* [S; T](
  self: Stream[S, T];
  condition: Predicate[T]
): Stream[S, T] =
  func buildDropLoop [S; T; L: LoopedCondition[S]](
    selfLoop: L;
    generator: Generator[S, T];
    condition: condition.typeof()
  ): L =
    selfLoop.focusOn(L.condition()).modify(
      (hasMore: Condition[S]) =>
        hasMore.`and`(generator.map(condition) as hasMore.typeof())
    )

  func buildResult [X: self.typeof()](
    self: X;
    condition: condition.typeof();
    S: typedesc
  ): X =
    self.focusOn(X.initialStep()).modify(
      (initialStep: IO[S]) =>
        initialStep.map(
          self
            .focusOn(X.loop().chain(Loop[S, T].loopedCondition()))
            .read()
            .buildDropLoop(self.focusOn(X.generator()).read(), condition)
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
        .focusOn(self.typeof().initialStep())
        .write(() => self.nextStep(step))
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
        .apply(found => self.closure.map(_ => found).run(step))

  self.dropWhile(not predicate).initialStep.map(findItem).run()


proc findFirst* [S; T](self: Stream[S, T]): Optional[T] =
  self.findFirst(alwaysTrue[T])



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "test":
      func indexes [I, T](a: array[I, T]): Stream[Natural, Natural] =
        itself((i: Natural) => i < a.len())
          .looped((i: Natural) => i + 1)
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
