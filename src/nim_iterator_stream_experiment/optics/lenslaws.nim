import lens
import ../monad/[reader]

import std/[macros, sugar]



export reader



type
  IdentitySpec* [S] = tuple
    expected: S

  RetentionSpec* [S; T] = tuple
    state: S
    expected: T

  DoubleWriteSpec* [S; T] = tuple
    state: S
    first: T
    second: T

  LensLawsSpec* [S; T] = tuple
    identity: IdentitySpec[S]
    retention: RetentionSpec[S, T]
    doubleWrite: DoubleWriteSpec[S, T]




func identitySpec* [S](expected: S): IdentitySpec[S] =
  (expected: expected)


func retentionSpec* [S; T](state: S; expected: T): RetentionSpec[S, T] =
  (state: state, expected: expected)


func doubleWriteSpec* [S; T](
  state: S;
  first: T;
  second: T
): DoubleWriteSpec[S, T] =
  (state: state, first: first, second: second)


func lensLawsSpec* [S; T](
  identity: IdentitySpec[S];
  retention: RetentionSpec[S, T];
  doubleWrite: DoubleWriteSpec[S, T]
): LensLawsSpec[S, T] =
  (identity: identity, retention: retention, doubleWrite: doubleWrite)



template checkIdentityLaw* [S; T](
  lens: Lens[S, T];
  spec: IdentitySpec[S]
): bool =
  lens
    .read()
    .flatMap((t: T) => lens.write(() => t))
    .run(spec.expected)
    .`==`(spec.expected)


template checkRetentionLaw* [S; T](
  lens: Lens[S, T];
  spec: RetentionSpec[S, T]
): bool =
  lens
    .write(() => spec.expected)
    .map(lens.read())
    .run(spec.state)
    .`==`(spec.expected)


template checkDoubleWriteLaw* [S; T](
  lens: Lens[S, T];
  spec: DoubleWriteSpec[S, T]
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
  import private/[lens_test_common]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    let testedLens = name(Street)



    test """Using "checkIdentityLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: IdentitySpec[S]) =
        check:
          lens.checkIdentityLaw(spec)


      doTest(testedLens, identitySpec(street("abc", 751)))



    test """Using "checkRetentionLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: RetentionSpec[S, T]) =
        check:
          lens.checkRetentionLaw(spec)


      doTest(testedLens, retentionSpec(street("2qfze26q", 9), "abc"))



    test """Using "checkDoubleWriteLaw" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: DoubleWriteSpec[S, T]) =
        check:
          lens.checkDoubleWriteLaw(spec)


      doTest(
        testedLens,
        doubleWriteSpec(street("abc", 13845), "ABC", "0123")
      )



    test """Using "checkLensLaws" in a unit test's "check" section should compile.""":
      proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
        check:
          lens.checkLensLaws(spec)


      doTest(
        testedLens,
        lensLawsSpec(
          identitySpec(street("t", 5)),
          retentionSpec(street("A0", 79), "street"),
          doubleWriteSpec(street("ac", 18996), "a", "b")
        )
      )
