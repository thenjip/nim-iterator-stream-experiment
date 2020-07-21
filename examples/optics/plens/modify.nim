import nim_iterator_stream_experiment/optics/[plens]
import
  nim_iterator_stream_experiment/optics/plens/private/test/[
    "block",
    conditionalblock
  ]
import nim_iterator_stream_experiment/utils/[call, chain]

import std/[strformat, sugar]



when isMainModule:
  proc main () =
    let
      input = conditionalBlock(() => false, `block`("a", () => 5))
      output =
        input.modify(
          input.typeof().thenBlock(float),
          old =>
            `block`(fmt"{old.label()}1", old.body().chain(i => i.toFloat()))
        )

    doAssert(input.condition() == output.condition())
    doAssert(output.thenLabel() == fmt"{input.thenLabel()}1")
    doAssert(output.thenBody().call() == input.thenBody().call().toFloat())



  main()
