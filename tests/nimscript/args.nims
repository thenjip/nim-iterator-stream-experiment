import nim_iterator_stream_experiment/streams/[slice]

import std/[sugar]



type
  ProgArgIndex = Natural
  ProgArgStep = SignedSliceStep



func progArgIndexes (): Stream[ProgArgStep, ProgArgIndex] =
  slice(0, paramCount() - 1).ordinals().map(i => i.Natural)


func progArgs (): Stream[ProgArgStep, string] =
  progArgIndexes().map(i => i.int.paramStr())


func progArgPairs (): Stream[ProgArgStep, tuple[i: ProgArgIndex; arg: string]] =
  progArgIndexes().map(i => (i, i.int.paramStr()))



when isMainModule:
  proc test1 () =
    let
      actual = progArgIndexes().count(Natural)
      expected = paramCount()

    doAssert(actual == expected)


  proc test2 () =
    func matchesExpectedArg (pair: tuple[i: ProgArgIndex; arg: string]): bool =
      pair.i.int.paramStr() == pair.arg

    doAssert(progArgPairs().all(matchesExpectedArg))


  proc test3 () =
    let
      actual =
        progArgs()
        .reduce(
          (s: seq[string], arg: string) => s & arg,
          newSeqOfCap[string](paramCount())
        )
      expected =
        collect newSeqOfCap(paramCount()):
          for i in 0 ..< paramCount():
            i.paramStr()

    doAssert(actual == expected)



  proc main () =
    test1()
    test2()
    test3()



  main()
