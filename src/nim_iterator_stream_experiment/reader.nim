import chain, identity

import std/[sugar]



type Reader[S, T] = S -> T



func ask* (S: typedesc): Reader[S, S] =
  itself[S]


func ask* [S](): Reader[S, S] =
  S.ask()



func toReader* [T](value: T; S: typedesc): Reader[S, T] =
  (_: S) => value


func toReader* [S; T](value: T): Reader[S, T] =
  value.toReader(S)



proc run* [S; T](self: Reader[S, T]; state: S): T =
  state.self()



func map* [S; A; B](self: Reader[S, A]; f: A -> B): Reader[S, B] =
  self.chain(f)


func flatMap* [S; A; B](
  self: Reader[S, A];
  f: A -> Reader[S, B]
): Reader[S, B] =
  (state: S) => self.run(state).f().run(state)



func local* [S; T](self: Reader[S, T]; f: S -> S): Reader[S, T] =
  ##[
    Returns a `Reader` that will execute `self` in an environment
    modified by `f`.
  ]##
  f.map(self)



when isMainModule:
  import io, unit

  import std/[os, unittest]



  template lazyCheck (checks: untyped): proc (): Unit =
    (
      proc (): Unit =
        check:
          checks
    )



  suite currentSourcePath().splitFile().name:
    test "ask":
      proc askTest [S](expected: S) =
        let
          sut1 = S.ask()
          sut2 = ask[S]()

        check:
          expected.sut1() == expected
          expected.sut2() == expected


      askTest(@[new string])
      askTest({0: 'a'})



    test "toReader":
      proc toReaderTest [S; T](expected: T; state: S) =
        let
          sut1 = expected.toReader(S)
          sut2 = expected.toReader[:S, T]()

        check:
          state.sut1() == expected
          state.sut2() == expected


      toReaderTest(0, "")



    test "flatMap":
      proc flatMapTest [A; B](initial: A; expected: B; S: typedesc) =
        let sut =
          initial
          .toReader(S)
          .flatMap(
            (a: A) =>
              lazyCheck(a == initial).map(_ => expected.toReader(S)).run()
          ).run(S.default())

        check:
          sut == expected


      flatMapTest(0, "a", seq[int32])



    test "map":
      proc mapTest [A; B](initial: A; expected: B; S: typedesc) =
        let sut =
          initial
          .toReader(S)
          .map(a => lazyCheck(a == initial).map(_ => expected).run())
          .run(S.default())

        check:
          sut == expected


      mapTest(0, () => 9u, array[0 .. 10, (string, uint)])



    test "local":
      proc checkState [S](T: typedesc; expectedState: S): T -> Reader[S, T] =
        (a: T) =>
          S
          .ask()
          .map(s => lazyCheck(s == expectedState).map(_ => a).run())


      proc localTest [S; T](
        expectedVal: T;
        initialState: S;
        modifiedState: S;
        f: S -> S
      ) =
        require:
          initialState != modifiedState

        let sut =
          expectedVal
          .toReader(S)
          .flatMap(T.checkState(modifiedState))
          .local(f)
          .flatMap(T.checkState(initialState))

        check:
          initialState.sut() == expectedVal


      localTest(0, "a", "ab", s => s & "b")
