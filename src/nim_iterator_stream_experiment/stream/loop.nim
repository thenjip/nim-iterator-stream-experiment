import loop/[loopscope]
import ../monad/[io, optional, predicate, reader]
import ../optics/[lens]
import ../utils/[convert, unit]

import std/[sugar]



type
  Generator* [S; T] = Reader[S, T]

  Loop* [S; T] = object
    scope: LoopScope[S]
    generator: Generator[S, T]



func generator* [S; T](g: Reader[S, T]): Generator[S, T] =
  g



template stepType* [S; T](X: typedesc[Loop[S, T]]): typedesc[S] =
  S


template stepType* [S; T](self: Loop[S, T]): typedesc[S] =
  self.typeof().stepType()


template itemType* [S; T](X: typedesc[Loop[S, T]]): typedesc[T] =
  T


template itemType* [S; T](self: Loop[S, T]): typedesc[T] =
  self.typeof().itemType()



func generating* [S; T](
  scope: LoopScope[S];
  generator: Generator[S, T]
): Loop[S, T] =
  Loop[S, T](scope: scope, generator: generator)



func scope* [S; T](X: typedesc[Loop[S, T]]): Lens[X, LoopScope[S]] =
  lens(
    (self: X) => self.scope,
    (self: X, scope: LoopScope[S]) => scope.generating(self.generator)
  )


func generator* [S; A](
  X: typedesc[Loop[S, A]];
  B: typedesc
): PLens[X, Generator[S, A], Generator[S, B], Loop[S, B]] =
  lens(
    (self: X) => self.generator,
    (self: X, generator: Generator[S, B]) => self.scope.generating(generator)
  )


func generator* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Generator[S, T]] =
  X.generator(T)


func condition* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Condition[S]] =
  X.scope().chain(LoopScope[S].condition())


func stepper* [S; T](X: typedesc[Loop[S, T]]): Lens[X, Stepper[S]] =
  X.scope().chain(LoopScope[S].stepper())



proc mapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  stepperMapper: Stepper[SA] -> Stepper[SB]
): Loop[SB, T] =
  self
    .scope
    .mapSteps(extractor, stepperMapper)
    .generating(extractor.map(self.generator))


proc mapSteps* [SA; SB; T](
  self: Loop[SA, T];
  extractor: SB -> SA;
  wrapper: SA -> SB
): Loop[SB, T] =
  self.mapSteps(extractor, stepper => extractor.map(stepper).map(wrapper))



func emptyLoop* (S: typedesc; T: typedesc): Loop[S, T] =
  S.emptyLoopScope().generating(nil as Generator[S, T])


func infiniteLoop* [S; T](
  stepper: Stepper[S];
  generator: Generator[S, T]
): Loop[S, T] =
  alwaysTrue[S].looped(stepper).generating(generator)


func infiniteLoop* [S; T](
  stepper: S -> S;
  generator: Generator[S, T]
): Loop[S, T] =
  stepper.convert(Stepper[S]).infiniteLoop(generator)



proc run* [S](self: Loop[S, Unit]; initial: S): S =
  self.scope.run(initial, self.generator)



func asReader* [S](self: Loop[S, Unit]): Reader[S, S] =
  self.scope.asReader(self.generator)



proc runOnce* [S; T](self: Loop[S, T]; start: S): RunOnceResult[S, T] =
  self.scope.runOnce(start, self.generator)



func map* [S; A; B](self: Loop[S, A]; f: A -> B): Loop[S, B] =
  self.scope.generating(self.generator.map(f))



func dropWhile* [S; T](
  self: Loop[S, T];
  predicate: Predicate[T]
): Reader[S, S] =
  self.scope.breakIf(self.generator.map(not predicate)).asReader()



when isMainModule:
  import ../monad/[identity]
  import ../optics/[lenslaws]
  import ../utils/[ignore, partialprocs, operators]

  import std/[os, sequtils, unittest]



  func indexLoop [T](s: seq[T]): Loop[Natural, Natural] =
    partial(?:Natural < s.len())
      .looped(plus1)
      .generating(itself[Natural])


  func itemLoop [T](s: seq[T]): Loop[Natural, T] =
    s.indexLoop().map(i => s[i])



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"Loop[S, T].scope()" should return a lens that obeys the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[Loop[S, T], LoopScope[S]]) =
          check:
            Loop[S, T].scope().checkLensLaws(spec)


        proc runTest1 () =
          let loop = itself[Unit].infiniteLoop((_: Unit) => 14u64)

          doTest(
            lensLawsSpec(
              identitySpec(loop),
              retentionSpec(loop, alwaysFalse[loop.stepType()].looped(itself)),
              doubleWriteSpec(
                loop,
                alwaysFalse[loop.stepType()].looped(itself),
                alwaysTrue[loop.stepType()].looped(itself)
              )
            )
          )


        runTest1()



      test """"Loop[S, T].generator()" should return a lens that obeys the lens laws.""":
        proc doTest [S; T](spec: LensLawsSpec[Loop[S, T], Generator[S, T]]) =
          check:
            Loop[S, T].generator().checkLensLaws(spec)


        proc runTest1 () =
          let loop = emptyLoop(Unit, seq[Unit])

          doTest(
            lensLawsSpec(
              identitySpec(loop),
              retentionSpec(
                loop,
                generator((_: loop.stepType()) => unit().repeat(5))
              ),
              doubleWriteSpec(
                loop,
                generator((_: loop.stepType()) => loop.itemType().default()),
                (_: loop.stepType()) => @[unit()]
              )
            )
          )


        runTest1()



      test """Dropping the items of a "seq[T]" while they verify the given predicate should return the index of the first item that does not verify the predicate.""":
        proc doTest [T](
          input: seq[T];
          predicate: Predicate[T];
          expected: Natural
        ) =
          let actual = input.itemLoop().dropWhile(predicate).run(input.low())

          check:
            actual == expected


        proc doTest [T](
          input: seq[T];
          predicate: Predicate[T];
          expectedIndex: seq[T] -> Natural
        ) =
          doTest(input, predicate, expectedIndex.run(input))


        doTest(seq[Unit].default(), alwaysTrue[Unit], partial(low(?_)))
        doTest(@["", "", "", "", "a", ""], (s: string) => s.len() == 0, 4)
        doTest(@[-5, -1, -3], partial(?:int > 0), partial(low(?_)))
        doTest(@[1.0, 15.47, 62.8], alwaysTrue[float], partial(len(?_)))



  main()
