##[
  See polymorphic lens `module <plens.html>`_ documentation.

  Example:
    .. code-block:: nim
      import nim_iterator_stream_experiment/optics/[lens]

      import std/[sugar]


      type
        HouseNumber = Natural
        RoadName = string

        RoadKind {.pure.} = enum
          Street
          Avenue
          Boulevard

        Road = object
          kind: RoadKind
          name: RoadName

        CityAddress = object
          number: HouseNumber
          road: Road



      func road (kind: RoadKind; name: RoadName): Road =
        Road(kind: kind, name: name)


      func kind [X: Road](): Lens[X, RoadKind] =
        lens(
          (self: X) => self.kind,
          (self: X, kind: RoadKind) => road(kind, self.name)
        )


      func name [X: Road](): Lens[X, RoadName] =
        lens(
          (self: X) => self.name,
          (self: X, name: RoadName) => road(self.kind, name)
        )



      func cityAddress (number: Natural; road: Road): CityAddress =
        CityAddress(number: number, road: road)


      func number [X: CityAddress](): Lens[X, HouseNumber] =
        lens(
          (self: X) => self.number,
          (self: X, number: HouseNumber) => cityAddress(number, self.road)
        )


      func road [X: CityAddress](): Lens[X, Road] =
        lens(
          (self: X) => self.road,
          (self: X, road: Road) => cityAddress(self.number, road)
        )


      func roadKind (X: typedesc[CityAddress]): Lens[X, RoadKind] =
        road[CityAddress]().chain(kind[Road]())


      func roadName (X: typedesc[CityAddress]): Lens[X, RoadName] =
        road[CityAddress]().chain(name[Road]())



      when isMainModule:
        import nim_iterator_stream_experiment/monad/[reader]

        import std/[strformat]



        proc main () =
          const
            expected = cityAddress(5, road(RoadKind.Boulevard, "abc somewhere"))
            actual =
              number[CityAddress]()
                .write(() => 5.HouseNumber)
                .map(CityAddress.roadName().modify(prev => fmt"{prev} somewhere"))
                .map(CityAddress.roadKind().write(() => RoadKind.Boulevard))
                .run(cityAddress(1, road(RoadKind.Street, "abc")))

          doAssert(actual == expected)



        main()
]##



import plens



export plens



type Lens* [S; T] = PLens[S, T, T, S]



template stateType* [S; T](X: typedesc[Lens[S, T]]): typedesc[S] =
  S


template memberType* [S; T](X: typedesc[Lens[S, T]]): typedesc[T] =
  T



template stateType* [S; T](self: Lens[S, T]): typedesc[S] =
  self.typeof().stateType()


template memberType* [S; T](self: Lens[S, T]): typedesc[T] =
  self.typeof().memberType()



when isMainModule:
  import private/[lens_test_common]
  import ../monad/[reader]
  import ../utils/[call, operators]

  import std/[os, sugar, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """Reading the "Road" name through a lens should return the same value as the member.""":
        proc doTest (nameLens: Lens[Road, RoadName]; road: Road) =
          let
            actual = nameLens.read().run(road)
            expected = road.name()

          check:
            actual == expected


        doTest(name[Road](), road(Street, "aaBc01a"))



      test """Modifying the "Road" kind through a lens should return a "Road" with only the modified kind.""":
        proc doTest (
          lens: Lens[Road, RoadKind];
          road: Road;
          modifier: RoadKind -> RoadKind
        ) =
          let
            actual = lens.modify(modifier).run(road)
            expected = road(road.kind().modifier(), road.name())

          check:
            actual == expected


        doTest(
          kind[Road](),
          road(RoadKind.Avenue, "aaa"),
          _ => RoadKind.Boulevard
        )



      test """Writing the "Road" name of a "CityAddress" through a lens should return a "CityAddress" with only the road name changed.""":
        proc doTest (
          lens: Lens[CityAddress, RoadName];
          input: CityAddress;
          newName: RoadName
        ) =
          require:
            input.road().name() != newName

          let
            actual = lens.write(() => newName).run(input)
            expected =
              cityAddress(input.number(), road(input.road().kind(), newName))

          check:
            actual == expected


        doTest(
          CityAddress.roadName(),
          cityAddress(0, road(RoadKind.Boulevard, "abc")),
          "bca"
        )



      test """Modifying the "Road" kind of a "CityAddress" through a lens at compile time should return a "CityAddress" with only the road kind changed.""":
        proc doTest (
          lens:
            static[
              proc (): Lens[CityAddress, RoadKind] {.nimcall, noSideEffect.}
            ]
          ;
          input: static CityAddress;
          modifier:
            static proc (old: RoadKind): RoadKind {.nimcall, noSideEffect.}
        ) =
          const
            actual = lens.call().modify(modifier).run(input)
            expected =
              cityAddress(
                input.number(),
                road(modifier.run(input.road().kind()), input.road().name())
              )

          check:
            actual == expected


        doTest(
          () => CityAddress.roadKind(),
          cityAddress(2, road(RoadKind.Street, "a a")),
          next[RoadKind]
        )



  main()
