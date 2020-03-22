import lens
import ../monad/[io, reader]

import std/[sugar]



template checkIdentityLaw* [S; T](lens: Lens[S, T]; expected: S): bool =
  lens.read().flatMap((t: T) => lens.write(t.toIO())).run(expected) == expected


template checkRetentionLaw* [S; T](
  lens: Lens[S, T];
  state: S;
  expected: T
): bool =
  lens.write(expected.toIO()).map(lens.read()).run(state) == expected


template checkDoubleSetLaw* [S; T](
  lens: Lens[S, T];
  state: S;
  firstValue: T;
  secondValue: T
): bool =
  lens
    .write(firstValue.toIO())
    .map(lens.write(secondValue.toIO()))
    .map(lens.read())
    .run(state)
    .`==`(secondValue)



when isMainModule:
  import lens_test_common

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "checkIdentityLaw":
      proc checkIdentityLawTest [S; T](lens: Lens[S, T]; expected: S) =
        check:
          lens.checkIdentityLaw(expected)


      checkIdentityLawTest(Street.focusOnName(), street("abc", 751))



    test "checkRetentionLaw":
      proc checkRetentionLawTest [S; T](
        lens: Lens[S, T];
        state: S;
        expected: T
      ) =
        check:
          lens.checkRetentionLaw(state, expected)


      checkRetentionLawTest(Street.focusOnName(), street("2qfze26q", 9), "abc")



    test "checkDoubleSetLaw":
      proc checkDoubleSetLawTest [S; T](
        lens: Lens[S, T];
        state: S;
        firstValue: T;
        secondValue: T
      ) =
        require:
          firstValue != secondValue

        check:
          lens.checkDoubleSetLaw(state, firstValue, secondValue)


      checkDoubleSetLawTest(
        Street.focusOnName(),
        street("abc", 13845),
        "ABC",
        "0123"
      )
