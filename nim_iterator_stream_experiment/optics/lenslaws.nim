##[
  Utilities to check if a `lens <lens.html>`_ verifies the lens laws.

  This module is meant to be used in test suites.

  `Examples <https://github.com/thenjip/nim-iterator-stream-experiment/tree/3938f1b6e1086c6c1799761941eb36a08be1c4c2/examples/optics/lenslaws>`_
]##



import lens
import ../monad/[reader]

import std/[macros, sugar]



export lens, reader



type
  IdentitySpec* [S] = tuple
    ##[
      Parameters for the lens identity law.
    ]##
    expected: S

  RetentionSpec* [S; T] = tuple
    ##[
      Parameters for the lens retention law.
    ]##
    input: S
    expected: T

  DoubleWriteSpec* [S; T] = tuple
    ##[
      Parameters for the lens double write law.
    ]##
    input: S
    first: T
    second: T

  LensLawsSpec* [S; T] = tuple
    identity: IdentitySpec[S]
    retention: RetentionSpec[S, T]
    doubleWrite: DoubleWriteSpec[S, T]




func identitySpec* [S](expected: S): IdentitySpec[S] =
  (expected: expected)


func retentionSpec* [S; T](input: S; expected: T): RetentionSpec[S, T] =
  (input: input, expected: expected)


func doubleWriteSpec* [S; T](
  input: S;
  first: T;
  second: T
): DoubleWriteSpec[S, T] =
  (input: input, first: first, second: second)


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
  ##[
    Checks whether a read followed by a write through `lens` is the same as not
    modifying the initial structure.
  ]##
  lens
    .read()
    .flatMap((t: T) => lens.write(() => t))
    .run(spec.expected)
    .`==`(spec.expected)


template checkRetentionLaw* [S; T](
  lens: Lens[S, T];
  spec: RetentionSpec[S, T]
): bool =
  ##[
    Checks whether a write followed by a read through `lens` returns the same
    value written.
  ]##
  lens
    .write(() => spec.expected)
    .map(lens.read())
    .run(spec.input)
    .`==`(spec.expected)


template checkDoubleWriteLaw* [S; T](
  lens: Lens[S, T];
  spec: DoubleWriteSpec[S, T]
): bool =
  ##[
    Checks whether two consecutive writes through `lens` is the same as directly
    writing the second value.
  ]##
  lens
    .write(() => spec.first)
    .map(lens.write(() => spec.second))
    .run(spec.input)
    .`==`(lens.write(() => spec.second).run(spec.input))



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
  import lens/private/test/[cityaddress, road]

  import std/[os, unittest]



  func brokenReadLens (): Lens[Road, RoadName] =
    name[Road]()
      .chain(
        lens((name: RoadName) => "", (_: RoadName, `new`: RoadName) => `new`)
      )


  func brokenWriteLens (): Lens[CityAddress, RoadKind] =
    lens(
      CityAddress.roadKind().read(),
      (address: CityAddress, _: RoadKind) => address
    )



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """Using "checkIdentityLaw" in a unit test's "check" section should compile.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: IdentitySpec[S]) =
          check:
            lens.checkIdentityLaw(spec)


        doTest(
          number[CityAddress](),
          identitySpec(cityAddress(751, road(RoadKind.Street, "abc")))
        )



      test """Using "checkRetentionLaw" in a unit test's "check" section should compile.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: RetentionSpec[S, T]) =
          check:
            lens.checkRetentionLaw(spec)


        doTest(
          number[CityAddress](),
          retentionSpec(
            cityAddress(9, road(RoadKind.Boulevard, "2qfze26q")),
            5.HouseNumber
          )
        )



      test """Using "checkDoubleWriteLaw" in a unit test's "check" section should compile.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: DoubleWriteSpec[S, T]) =
          check:
            lens.checkDoubleWriteLaw(spec)


        doTest(
          number[CityAddress](),
          doubleWriteSpec(
            cityAddress(13845, road(RoadKind.Avenue, "abc")),
            98.HouseNumber,
            123.HouseNumber
          )
        )



      test """Using "checkLensLaws" in a unit test's "check" section should compile.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
          check:
            lens.checkLensLaws(spec)


        doTest(
          number[CityAddress](),
          lensLawsSpec(
            identitySpec(cityAddress(5, road(RoadKind.Boulevard, "t"))),
            retentionSpec(
              cityAddress(79, road(RoadKind.Avenue, "A0")),
              0.HouseNumber
            ),
            doubleWriteSpec(
              cityAddress(18996, road(RoadKind.Street, "ac")),
              1.HouseNumber,
              11.HouseNumber
            )
          )
        )



      test """A lens with a deficient member reader should not verify at least one of the laws.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
          check:
            not lens.checkLensLaws(spec)


        doTest(
          brokenReadLens(),
          lensLawsSpec(
            identitySpec(road(RoadKind.Street, "abc")),
            retentionSpec(road(RoadKind.Boulevard, "0123"), "a0b1"),
            doubleWriteSpec(road(RoadKind.Avenue, "bcd"), "aa", "00")
          )
        )



      test """A lens with a deficient member writer should not verify at least one of the laws.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
          check:
            not lens.checkLensLaws(spec)


        doTest(
          brokenWriteLens(),
          lensLawsSpec(
            identitySpec(cityAddress(0, road(RoadKind.Avenue, "a"))),
            retentionSpec(
              cityAddress(3, road(RoadKind.Boulevard, "01")),
              RoadKind.Street
            ),
            doubleWriteSpec(
              cityAddress(142, road(RoadKind.Street, "aaa")),
              RoadKind.Avenue,
              RoadKind.Boulevard
            )
          )
        )



  main()
