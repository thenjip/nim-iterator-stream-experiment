import lens

import std/[sugar]



type
  Street* = object
    name: string
    number: Natural



func street* (name: string; number: Natural): Street =
  Street(name: name, number: number)



func readName* (self: Street): string =
  self.name


func readNumber* (self: Street): Natural =
  self.number



func name* (X: typedesc[Street]): Lens[X, string] =
  lens(
    readName,
    (self: X, name: self.name.typeof()) => name.street(self.number)
  )
