#[
  This can compiled to C or C++ and checked for file descriptor leaks with
  ``valgrind --track-fds=yes``. No leaks should be found.
]#



import nim_iterator_stream_experiment/monad/[io]
import nim_iterator_stream_experiment/utils/[unit]

import std/[sugar]



proc withFile* [T](file: () -> File; compute: File -> T): IO[T] =
  file.tryBracket(compute, proc (f: File): Unit = f.close())


proc openCurrentSrcFile* (): File =
  currentSourcePath().open()



when isMainModule:
  proc main () =
    let fileSize = openCurrentSrcFile.withFile(getFileSize).run()

    echo(fileSize) # This should print a positive number.



  main()
