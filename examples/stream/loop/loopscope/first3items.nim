import countup
import nim_iterator_stream_experiment/stream/loop/[loopscope]

import std/[sugar]



func first3* [I: SomeInteger](high: I): LoopScope[I] =
  high.countUp().breakIf((i: I) => i >= 3)



when isMainModule:
  import nim_iterator_stream_experiment/utils/[ignore, unit]



  proc collectFrom [I: SomeInteger; T](scope: LoopScope[I]; s: seq[T]): seq[T] =
    var collection: seq[T] = @[]

    scope.run(s.low(), proc (i: I): Unit = collection.add(s[i])).ignore()

    collection


  proc main () =
    let
      someSeq = @[1, 9, 5, -1, 7]
      expected = someSeq[0 .. 2]
      collected = someSeq.high().first3().collectFrom(someSeq)

    doAssert(collected == expected)



  main()
