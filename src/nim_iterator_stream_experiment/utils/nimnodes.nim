import std/[macros]



func low* (n: NimNode): Natural =
  0


func high* (n: NimNode): Natural =
  n.len().pred()


func firstChild* (n: NimNode): NimNode =
  n[n.low()]


func secondChild* (n: NimNode): NimNode =
  n[n.low().succ()]



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
