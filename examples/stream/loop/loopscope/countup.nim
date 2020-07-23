import nim_iterator_stream_experiment/stream/loop/[loopscope]
import nim_iterator_stream_experiment/utils/[operators]

import std/[sugar]



func countUp* [I: SomeInteger](high: I): LoopScope[I] =
  looped((i: I) => i <= high, next)



when isMainModule:
  import nim_iterator_stream_experiment/utils/[ignore, unit]

  import std/[sequtils]



  proc collectToSeq [T](
    scope: LoopScope[T];
    start: T;
    initLen: Natural
  ): seq[T] =
    var collection = newSeqOfCap[T](initLen)

    scope.run(start, proc (item: auto): Unit = collection.add(item)).ignore()

    collection


  proc main () =
    let
      slice = 0 .. 9
      collected = slice.b.countUp().collectToSeq(slice.a, slice.len())

    doAssert(collected == toSeq(slice.items()))



  main()
