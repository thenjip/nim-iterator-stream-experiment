import slice
import ../stream
import ../utils/[nimnodes]

import std/[macros, sugar]



type
  NimNodeStep* = SliceStep[NimNodeIndex]



func indexes* (n: NimNode): Stream[NimNodeStep, NimNodeIndex] =
  slice(n.low(), n.high()).items()


func children* (n: NimNode): Stream[NimNodeStep, NimNode] =
  n.indexes().map(i => n[i])


func pairs* (
  n: NimNode
): Stream[NimNodeStep, tuple[index: NimNodeIndex; node: NimNode]] =
  n.indexes().map(i => (i, n[i]))



when isMainModule:
  import ../utils/[call]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """Counting the number of children of a NimNode "n" through a stream should return "n.len()".""":
      template doTest (n: NimNode): proc () {.nimcall.} =
        (
          proc () =
            const
              actual = n.children().count(Natural)
              expected = n.len()

            check:
              actual == expected
        )


      for t in [
        doTest(newEmptyNode()),
        doTest(
          quote do:
            var a = nil
            let b = a

            echo(b)
        )
      ]:
        t.call()
