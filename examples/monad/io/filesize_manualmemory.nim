#[
  This can compiled to C or C++ and checked for file descriptor leaks with
  ``valgrind --track-fds=yes``. No leaks should be found.
]#



import filesize, manualmemory

import nim_iterator_stream_experiment/monad/[io]
import nim_iterator_stream_experiment/utils/[lambda]

import std/[sugar]



proc readCurrentSrcFileSize* (): int64 =
  openCurrentSrcFile.withFile(getFileSize).run()


proc countDigits* [I: SomeInteger](i: I): Natural =
  i.createInit().lambda().withMemory(i => len($i)).run()



when isMainModule:
  proc main () =
    let nDigits = readCurrentSrcFileSize.map(countDigits).run()

    echo(nDigits)



  main()
