type Unit* = tuple[]



func unit* (): Unit =
  ()


func default* (T: typedesc[Unit]): Unit =
  unit()


func default* [T: Unit](): Unit =
  T.default()



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
