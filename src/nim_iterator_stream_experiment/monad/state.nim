import ../utils/[chain, unit]

import std/[sugar]



type
  StatePair* [S, T] = tuple[state: S; value: T]
  State* [S, T] = S -> StatePair[S, T]



func toState* [T](value: T; S: typedesc): State[S, T] =
  (state: S) => (state, value)


func toState* [S; T](value: T): State[S, T] =
  value.toState(S)



proc run* [S; T](self: State[S, T]; state: S): StatePair[S, T] =
  state.self()



func flatMap* [S; A; B](self: State[S, A]; f: A -> State[S, B]): State[S, B] =
  self.chain(pair => pair.value.f().run(pair.state))


func map* [S; A; B](self: State[S, A]; f: A -> B): State[S, B] =
  self.chain(pair => (pair.state, pair.value.f()))



func get* (S: typedesc): State[S, S] =
  (state: S) => (state, state)


func get* [S](): State[S, S] =
  S.get()



func put* [S](state: S): State[S, Unit] =
  (_: S) => (state, unit())



when isMainModule:
  import std/[os, sequtils, unittest]



  suite currentSourcePath().splitFile().name:
    test "monad law 1: left identity":
      proc leftIdentityTest [A; B](initialValue: A; f: A -> State[Unit, B]) =
        let
          expected = initialValue.f().run(unit())
          sut = initialValue.toState(Unit).flatMap(f).run(unit())

        check:
          sut == expected


      leftIdentityTest(-1, (i: int) => (i * -1).toState(Unit))
      leftIdentityTest("abc", (s: string) => s.len().toState(Unit))
      leftIdentityTest(
        @[1.2, 6.8, NegInf, 156.94],
        (s: seq[float]) => s.filter(f => f > 0.0).toState(Unit)
      )



    test "monad law 2: right identity":
      proc righIdentityTest [T](expectedValue: T) =
        let
          expected = expectedValue.toState(Unit)
          sut = expected.flatMap(toState[Unit, T])

        check:
          sut.run(unit()) == expected.run(unit())


      righIdentityTest(16)
      righIdentityTest([-568 .. 961])
      righIdentityTest("abc abc")
      righIdentityTest(new float)



    test "monad law 3: associativity":
      proc associativityTest [A; B; C](
        initialValue: A;
        f: A -> State[Unit, B];
        g: B -> State[Unit, C]
      ) =
        let
          expected = initialValue.toState(Unit).flatMap(f).flatMap(g)
          sut = initialValue.toState(Unit).flatMap((a: A) => a.f().flatMap(g))

        check:
          sut.run(unit()) == expected.run(unit())


      associativityTest(
        1388976,
        (i: int) => i.`$`().toState(Unit),
        (s: string) => s.len().toState(Unit)
      )



    test "get":
      proc getTest [S](expectedState: S) =
        let
          expected = (expectedState, expectedState)
          sut = S.get()

        check:
          expectedState.sut() == expected


      getTest([0.0, Inf])



    test "put":
      proc putTest [S](expectedState, unexpectedState: S) =
        require:
          expectedState != unexpectedState

        let
          expected = (expectedState, unit())
          sut = expectedState.put()

        check:
          unexpectedState.sut() == expected


      putTest("a", "b")
