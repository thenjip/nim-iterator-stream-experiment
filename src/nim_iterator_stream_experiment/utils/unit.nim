type Unit* = tuple[]



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



  suite currentSourcePath().splitFile().name:
    test "Unit.default() == unit()":
      check:
        Unit.default() == unit()
        default[Unit]() == unit()
