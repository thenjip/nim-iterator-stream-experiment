import identity
import ../utils/[call, chain]

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
  self.call(state)



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
  import lazymonadlaws
  import ../utils/[operators, partialprocs, proctypes, unit]

  import std/[os, sequtils, unittest]



  static:
    doAssert(
      Reader[cuint, ref NilAccessError] is LazyMonad[ref NilAccessError, cuint]
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
            partial(toReader(?_, Unit)),
            s => s.foldl(a + b.len().uint, 0u).toReader(Unit),
            unit()
          ),
          rightIdentitySpec({0..9}, partial(toReader(?_, cfloat)), 165.8),
          associativitySpec(
            ([0, 7], 'a'),
            partial(toReader(?_, string)),
            t => t[0].foldl(a + b, 0).plus(t[1].ord()).toReader(string),
            (i: int) => i.modulo(2).toReader(string),
            "0123"
          )
        )
      )



    test """"Reader[S, T]" without side effects should be compatible with compile time execution.""":
      template doTest [S; T](
        sut: Reader[S, T]{noSideEffect};
        state: static S;
        expected: static T
      ): proc () {.nimcall.} =
        (
          proc () =
            const actual = sut.run(state)

            check:
              actual == expected
        )


      func expected1 (): string =
        "abc"

      func sut1 (): Reader[Unit, expected1.returnType()] =
        (_: Unit) => expected1.call()


      doTest(sut1.call(), unit(), expected1.call()).call()



    test """"S.ask()" should give access to the read state when using "flatMap".""":
      proc doTest [S; T](self: Reader[S, T]; expected: S) =
        let actual = self.flatMap((_: T) => S.ask()).run(expected)

        check:
          actual == expected


      doTest(partial(len(?:string)), "abc")
