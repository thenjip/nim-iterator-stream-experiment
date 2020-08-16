import std/[os, strformat, sequtils, sugar]



func toNimLongOption* (name, value: string): string =
  fmt"--{name}:{value.quoteShell()}"



func toNimLongOptions* (
  pairs: openArray[tuple[name, value: string]]
): seq[string] =
  pairs.map(pair => pair.name.toNimLongOption(pair.value))
