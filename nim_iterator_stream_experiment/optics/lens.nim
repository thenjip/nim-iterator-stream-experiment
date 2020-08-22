##[
  See `polymorphic lenses <plens.html>`_.

  Examples:
    - Define lenses for a structure type.

      See `road <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/nim_iterator_stream_experiment/optics/lens/private/test/road.nim>`_ and `cityaddress <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/nim_iterator_stream_experiment/optics/lens/private/test/cityaddress.nim>`_.

    - Using lenses to `write parts of a structure <https://github.com/thenjip/nim-iterator-stream-experiment/blob/master/examples/optics/lens/write.nim>`_.
]##



import plens
import ../monad/[reader]



export plens, reader



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
  import lens/private/test/[cityaddress, road]
  import ../monad/[reader]
  import ../utils/[call, operators]

  import std/[os, sugar, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """Reading the "Road" name through a lens should return the same value as the member.""":
        proc doTest (lens: Lens[Road, RoadName]; input: Road) =
          let
            actual = lens.read().run(input)
            expected = input.name()

          check:
            actual == expected


        doTest(name[Road](), road(Street, "aaBc01a"))



      test """Writing the "Road" name of a "CityAddress" through a lens should return a "CityAddress" with only the input name changed.""":
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



      test """Modifying the "Road" kind through a lens should return a "Road" with only the kind modified.""":
        proc doTest (
          lens: Lens[Road, RoadKind];
          modifier: RoadKind -> RoadKind;
          input: Road
        ) =
          let
            actual = lens.modify(modifier).run(input)
            expected = road(input.kind().modifier(), input.name())

          check:
            actual == expected


        doTest(
          kind[Road](),
          _ => RoadKind.Boulevard,
          road(RoadKind.Avenue, "aaa")
        )



      test """Modifying the "Road" kind of a "CityAddress" through a lens at compile time should return a "CityAddress" with only the input kind changed.""":
        when defined(js):
          skip()
        else:
          proc doTest (
            lens:
              static[
                proc (): Lens[CityAddress, RoadKind] {.nimcall, noSideEffect.}
              ]
            ;
            modifier:
              static proc (old: RoadKind): RoadKind {.nimcall, noSideEffect.};
            input: static CityAddress
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
            next[RoadKind],
            cityAddress(2, road(RoadKind.Street, "a a"))
          )



  main()
