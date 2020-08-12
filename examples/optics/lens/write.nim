import nim_iterator_stream_experiment/optics/[lens]
import
  nim_iterator_stream_experiment/optics/lens/private/test/[road, cityaddress]
import nim_iterator_stream_experiment/monad/[reader]



when isMainModule:
  proc main () =
    let
      start = cityAddress(2, road(RoadKind.Avenue, "somewhere"))
      `end` = start.write(start.typeof().roadName(), "elsewhere")

    doAssert(start.number() == `end`.number())
    doAssert(start.road().kind() == `end`.road().kind())
    doAssert(start.road().name() != `end`.road().name())



  main()
