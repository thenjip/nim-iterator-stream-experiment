import nim_iterator_stream_experiment/monad/[optional, predicate, reader]
import nim_iterator_stream_experiment/utils/[partialprocs]

import std/[sequtils, sugar]



func mean [I: SomeInteger](s: seq[I]): Optional[I] =
  partial(?:s.len().typeof() == 0)
    .ifElse(_ => I.toNone(), n => n.I.toSome())
    .run(s.len())
    .map(n => s.foldl(a + b, 0.I) div n)



when isMainModule:
  proc main () =
    doAssert(@[].seq[:int].mean().isNone())
    doAssert(@[5, 10, 15, 20].mean().unbox() == 12)



  main()
