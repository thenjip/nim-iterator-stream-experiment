when isMainModule:
  import nim_iterator_stream_experiment/streams/[ast]
  import nim_iterator_stream_experiment/utils/[call]

  import std/[macros, sugar]



  template test1 (n: NimNode): proc () =
    (
      proc () =
        const
          actual = n.children().count(Natural)
          expected = n.len().Natural

        doAssert(actual == expected)
    )


  template test2 (n: NimNode): proc () =
    (
      proc () =
        const verified =
          n.pairs().all(
            (pair: (NimNodeIndex, NimNode)) => pair[1] == n[pair[0]]
          )

        doAssert(verified)
    )



  proc main () =
    for t in [
      test1(newEmptyNode()),
      test1(
        quote do:
          var a = nil
          let b = a

          echo(b)
      )
    ]:
      t.call()

    for t in [
      test2(newEmptyNode()),
      test2("abc".newStrLitNode()),
      test2(newProc())
    ]:
      t.call()



  main()
