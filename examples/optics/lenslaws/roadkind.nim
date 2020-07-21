import nim_iterator_stream_experiment/optics/[lenslaws]
import nim_iterator_stream_experiment/optics/lens/private/test/[road]

import std/[unittest]



when isMainModule:
  proc main () =
    suite """"Road" kind lens""":
      test """"kind[Road]()" should verify the lens laws.""":
        proc doTest [S; T](lens: Lens[S, T]; spec: LensLawsSpec[S, T]) =
          check:
            lens.checkLensLaws(spec)


        doTest(
          kind[Road](),
          lensLawsSpec(
            identitySpec(road(RoadKind.Street, "aaa")),
            retentionSpec(road(RoadKind.Avenue, "  "), RoadKind.Street),
            doubleWriteSpec(
              road(RoadKind.Avenue, "&&&"),
              RoadKind.Boulevard,
              RoadKind.Street
            )
          )
        )



  main()
