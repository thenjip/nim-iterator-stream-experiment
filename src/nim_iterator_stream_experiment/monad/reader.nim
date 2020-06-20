import identity
import ../utils/[chain]

import std/[sugar]



type Reader* [S, T] = S -> T



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
  import io, lazymonadlaws
  import ../utils/[unit]

  import std/[os, sequtils, unittest]



  static:
    doAssert(
      Reader[cuint, ref NilAccessError] is LazyMonad[ref NilAccessError, cuint]
    )



  template lazyCheck (checks: untyped): proc (): Unit =
    (
      proc (): Unit =
        check:
          checks
    )



  suite currentSourcePath().splitFile().name:
    test """"Reader[S, T]" should obey the monad laws.""":
      proc doTest [LA; LMA; LMB; LR; RT; RM; RR; AA; AB; AMA; AMB; AMC; AR](
        spec: MonadLawsSpec[LA, LMA, LMB, LR, RT, RM, RR, AA, AB, AMA, AMB, AMC, AR]
      ) =
        check:
          spec.checkMonadLaws()


      doTest(
        monadLawsSpec(
          leftIdentitySpec(
            @["a", "abc", "012"],
            a => a.toReader(Unit),
            s => s.foldl(a + b.len().uint, 0u).toReader(Unit),
            unit()
          ),
          rightIdentitySpec({0..9}, a => a.toReader(cfloat), 165.8),
          associativitySpec(
            ([0, 7], 'a'),
            a => a.toReader(string),
            t => toReader(t[0].foldl(a + b, 0) + t[1].ord(), string),
            (i: int) => i.`mod`(2).toReader(string),
            "0123"
          )
        )
      )



    test """"S.ask()" or "ask[S]" should return a "Reader[S, S]" that returns the passed argument.""":
      proc doTest [S](expected: S) =
        let
          sut1 = S.ask()
          sut2 = ask[S]()

        check:
          sut1.run(expected) == expected
          sut2.run(expected) == expected


      doTest(@[new string])
      doTest({0: 'a'})



    test """"S.ask()" should give access to the read state when using "flatMap".""":
      proc doTest [S; T](initial: Reader[S, T]; expected: S) =
        let sut = initial.flatMap((_: T) => S.ask())

        check:
          sut.run(expected) == expected


      doTest((s: string) => s.len(), "abc")



    test "local":
      proc checkState [S](T: typedesc; expectedState: S): T -> Reader[S, T] =
        (a: T) =>
          S
          .ask()
          .map(s => lazyCheck(s == expectedState).map(_ => a).run())


      proc doTest [S; T](
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


      doTest(0, "a", "ab", s => s & "b")
