import nim_iterator_stream_experiment/monad/[optional]
import nim_iterator_stream_experiment/utils/[call, ignore, unit]

import std/[sugar]



type Listener = () -> Unit



proc fireEvent (s: seq[Listener]) =
  for l in s:
    l.toOptional().map(p => p.call()).ignore() # Let "Optional" deal with "nil".



when isMainModule:
  proc main () =
    var called = false

    let listeners =
      @[
        proc (): Unit {.closure.} = echo("start"),
        proc (): Unit = called = true,
        nil,
        proc (): Unit = echo("end")
      ]

    listeners.fireEvent()

    doAssert(called)



  main()
