import std/[macros]



type
  NimNodeIndex* = int



func low* (n: NimNode): NimNodeIndex =
  0


func high* (n: NimNode): NimNodeIndex =
  n.len().pred()


func firstChild* (n: NimNode): NimNode =
  n[n.low()]


func secondChild* (n: NimNode): NimNode =
  n[n.low().succ()]



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
