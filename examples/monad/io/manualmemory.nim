#[
  This can compiled to C or C++ and checked for memory leaks with ``valgrind``.
  No leaks should be found.
]#



import nim_iterator_stream_experiment/monad/[io]
import nim_iterator_stream_experiment/utils/[chain, lambda, unit]

import std/[sugar]



proc derefer [T](p: ptr T): T =
  p[]


proc withMemory* [M; T](mem: () -> ptr M; f: M -> T): IO[T] =
  mem.tryBracket(derefer[M].chain(f), proc (m: ptr M): Unit = m.dealloc())


proc createInit* [T](init: T): ptr T =
  T
    .create()
    .lambda()
    .bracket(m => m, proc (m: ptr T): Unit = m[] = init)
    .run()



when isMainModule:
  proc main () =
    let nDigits = 12u.createInit().lambda().withMemory(i => len($i)).run()

    echo(nDigits) # This should print: 2



  main()
