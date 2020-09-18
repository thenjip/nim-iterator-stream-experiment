##[
  The `Unit` type from functional programming.

  It can be seen as the `void` type but with a single value in it.
]##



type
  Unit* = tuple[]



func unit* (): Unit =
  ()


func default* (T: typedesc[Unit]): Unit =
  unit()


func default* [T: Unit](): Unit =
  T.default()



func doNothing* [T](_: T): Unit =
  unit()



when isMainModule:
  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      test """"Unit.default()" should be equal to "unit()".""":
        proc doTest () =
          let
            actual1 = Unit.default()
            actual2 = default[Unit]()
            expected = unit()

          check:
            actual1 == expected
            actual2 == expected


        doTest()



  main()
