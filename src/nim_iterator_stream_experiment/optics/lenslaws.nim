import lens
import ../monad/[reader]

import std/[macros, sugar]



type
  IdentityLawSpec* [S] = tuple
    expected: S

  RetentionLawSpec* [S; T] = tuple
    state: S
    expected: T

  DoubleWriteLawSpec* [S; T] = tuple
    state: S
    first: T
    second: T

  LensLawsSpec* [S; T] = tuple
    identity: IdentityLawSpec[S]
    retention: RetentionLawSpec[S, T]
    doubleWrite: DoubleWriteLawSpec[S, T]




func identityLawSpec* [S](expected: S): IdentityLawSpec[S] =
  (expected: expected)


func retentionLawSpec* [S; T](state: S; expected: T): RetentionLawSpec[S, T] =
  (state: state, expected: expected)


func doubleWriteLawSpec* [S; T](
  state: S;
  first: T;
  second: T
): DoubleWriteLawSpec[S, T] =
  (state: state, first: first, second: second)


func lensLawsSpec* [S; T](
  identity: IdentityLawSpec[S];
  retention: RetentionLawSpec[S, T];
  doubleWrite: DoubleWriteLawSpec[S, T]
): LensLawsSpec[S, T] =
  (identity: identity, retention: retention, doubleWrite: doubleWrite)



template checkIdentityLaw* [S; T](
  lens: Lens[S, T];
  spec: IdentityLawSpec[S]
): bool =
  lens
    .read()
    .flatMap((t: T) => lens.write(() => t))
    .run(spec.expected)
    .`==`(spec.expected)


template checkRetentionLaw* [S; T](
  lens: Lens[S, T];
  spec: RetentionLawSpec[S, T]
): bool =
  lens
    .write(() => spec.expected)
    .map(lens.read())
    .run(spec.state)
    .`==`(spec.expected)


template checkDoubleWriteLaw* [S; T](
  lens: Lens[S, T];
  spec: DoubleWriteLawSpec[S, T]
): bool =
  lens
    .write(() => spec.first)
    .map(lens.write(() => spec.second))
    .run(spec.state)
    .`==`(lens.write(() => spec.second).run(spec.state))



template checkLensLaws* [S; T](
  lens: Lens[S, T];
  spec: LensLawsSpec[S, T]
): bool =
  `==`(
    (
      identity: lens.checkIdentityLaw(spec.identity),
      retention: lens.checkRetentionLaw(spec.retention),
      doubleWrite: lens.checkDoubleWriteLaw(spec.doubleWrite)
    ),
    (identity: true, retention: true, doubleWrite: true)
  )



when isMainModule:
  import lens_test_common

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    let testedLens = name(Street)



    test """Using "checkIdentityLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: IdentityLawSpec[S]) =
        check:
          lens.checkIdentityLaw(spec)


      doTest(testedLens, identityLawSpec(street("abc", 751)))



    test """Using "checkRetentionLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: RetentionLawSpec[S, T]) =
        check:
          lens.checkRetentionLaw(spec)


      doTest(testedLens, retentionLawSpec(street("2qfze26q", 9), "abc"))



    test """Using "checkDoubleWriteLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: DoubleWriteLawSpec[S, T]) =
        check:
          lens.checkDoubleWriteLaw(spec)


      doTest(
        testedLens,
        doubleWriteLawSpec(street("abc", 13845), "ABC", "0123")
      )



    test """Using "checkLensLaws" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
        check:
          lens.checkLensLaws(spec)


      doTest(
        testedLens,
        lensLawsSpec(
          identityLawSpec(street("t", 5)),
          retentionLawSpec(street("A0", 79), "street"),
          doubleWriteLawSpec(street("ac", 18996), "a", "b")
        )
      )
