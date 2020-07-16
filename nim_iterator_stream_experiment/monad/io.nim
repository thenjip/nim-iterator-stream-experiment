##[
  The IO monad from Haskell.

  It lets one build a computation that has no parameters.
  It can also be used as a wrapper for computations that interact with external
  resources such as memory management, FFI, I/O, etc. .

  Examples:
    - Open the currently compiled source file to read its size in bytes, then close it.

      .. code-block:: nim
        import nim_iterator_stream_experiment/monad/[io]
        import nim_iterator_stream_experiment/utils/[unit]

        import std/[sugar]


        proc withFile [T](file: () -> File; f: File -> T): IO[T] =
          file.bracket(f, proc (f: File): Unit = f.close())

        proc openCurrentSrcFile (): File =
          currentSourcePath().open()


        when isMainModule:
          let fileSize = openCurrentSrcFile.withFile(getFileSize).run()

          echo(fileSize) # This should print a natural.

      This can compiled to C or C++ and checked for file descriptor leaks with
      ``valgrind --track-fds=yes``. No leaks should be found.

    - Use manual memory management.

      .. code-block:: nim
        import nim_iterator_stream_experiment/monad/[io]
        import nim_iterator_stream_experiment/utils/[chain, lambda, unit]

        import std/[sugar]


        proc derefer [T](p: ptr T): T =
          p[]

        proc withMemory [M; T](mem: () -> ptr M; f: M -> T): IO[T] =
          mem.bracket(derefer[M].chain(f), proc (m: ptr M): Unit = m.dealloc())

        proc createInit [T](init: T): ptr T =
          T
            .create()
            .lambda()
            .bracket(m => m, proc (m: ptr T): Unit = m[] = init)
            .run()


        when isMainModule:
          let nDigits = 12u.createInit().lambda().withMemory(i => len($i)).run()

          echo(nDigits) # This should print: 2

      This can compiled to C or C++ and checked for memory leaks with
      ``valgrind``. No leaks should be found.

    - Chain the first example with the second one.

      .. code-block:: nim
        # Assuming the procedures and imports in the previous examples are declared here.

        proc readCurrentSrcFileSize (): int64 =
          openCurrentSrcFile.withFile(getFileSize).run()

        proc countDigits [I: SomeInteger](i: I): Natural =
          i.createInit().lambda().withMemory(i => len($i)).run()


        when isMainModule:
          let nDigits = readCurrentSrcFileSize.map(countDigits).run()

          echo(nDigits)
]##



import reader
import ../utils/[chain, unit]

import std/[sugar]



type IO* [T] = () -> T



func toIO* [T](value: T): IO[T] =
  () => value



proc run* [T](self: IO[T]): T =
  self()



func map* [A; B](self: IO[A]; f: A -> B): IO[B] =
  self.chain(f)


func flatMap* [A; B](self: IO[A]; f: A -> IO[B]): IO[B] =
  self.map(f).chain(run)



func bracket* [A; B](before: IO[A]; between: A -> B; after: A -> Unit): IO[B] =
  ##[
    An adaptation of stack management based on scopes for other resources like
    files, manually allocated memory blocks, ... .

    Here, the scope starts with `before` and ends with `after`.

    If an exception is raised anywhere in `before` or `between`, `after` will
    not be executed.
  ]##
  before.map(between.flatMap((b: B) => after.chain(_ => b)))



when isMainModule:
  import lazymonadlaws
  import ../utils/[call, lambda, proctypes, variables]

  import std/[os, sequtils, unittest]



  proc run [T](self: IO[T]; _: Unit): T =
    self.run()



  static:
    doAssert(IO[byte] is LazyMonad[byte, Unit])



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"IO[T]" should obey the monad laws.""":
        proc doTest [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC](
          spec: MonadLawsSpec[LA, LMA, LMB, Unit, RT, RM, Unit, AA, AB, AMA, AMB, AMC, Unit]
        ) =
          check:
            spec.checkMonadLaws()


        doTest(
          monadLawsSpec(
            leftIdentitySpec(-5, toIO, i => toIO(-i), unit()),
            rightIdentitySpec(() => newStringOfCap(10), toIO, unit()),
            associativitySpec(
              ('a', true),
              toIO,
              t => (t[0], not t[1]).toIO(),
              (t: (char, bool)) => t[0].`$`().len().toIO(),
              unit()
            )
          )
        )



      test """"IO[T]" without side effects should be compatible with compile time execution.""":
        template doTest [T](
          sut: IO[T]{noSideEffect};
          expected: static T
        ): proc () =
          (
            proc () =
              const actual = sut.run()

              check:
                actual == expected
          )


        func expected1 (): int =
          1


        func expected2 (): int =
          toSeq(1 .. 10).foldl(a + b, 0)

        func sut2 (): IO[expected2.returnType()] =
          lambda(1 .. 10)
            .map(slice => toSeq(slice.items()))
            .flatMap((s: seq[int]) => s.foldl(a + b, 0).toIO())


        for t in [
          doTest(expected1, expected1.call()),
          doTest(sut2.call(), expected2.call())
        ]:
          t.call()



      test """"after" in "self.bracket(between, after)" should be executed after "between".""":
        proc doTest () =
          var address = new byte

          let
            expected = address.read()
            actual =
              unit()
                .toIO()
                .bracket(
                  _ => address.read(),
                  _ => address.write(nil).doNothing()
                ).run()

          check:
            actual == expected



        doTest()



  main()
