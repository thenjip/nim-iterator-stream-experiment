import nim_iterator_stream_experiment/stream/loop/[loopscope]
import nim_iterator_stream_experiment/utils/[ignore, operators, unit]

import std/[random, sugar]



proc generateRandomInts [T: SomeInteger](
  seed: int64;
  limit: Natural;
  body: T -> Unit
) =
  var rng = initRand(seed)

  looped((i: limit.typeof()) => i < limit, next)
    .run(0, _ => rng.rand(T.low() .. T.high()).body())
    .ignore()



when isMainModule:
  proc main () =
    123.generateRandomInts(20, proc (n: uint32): Unit = echo(n))



  main()
