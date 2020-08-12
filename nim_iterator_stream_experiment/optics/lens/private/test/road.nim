import ../../../lens

import std/[sugar]



type
  RoadName* = string
  RoadKind* {.pure.} = enum
    Street
    Avenue
    Boulevard

  Road* = object
    kind: RoadKind
    name: RoadName



func road* (kind: RoadKind; name: RoadName): Road =
  Road(kind: kind, name: name)


func name* (self: Road): RoadName =
  self.name


func kind* (self: Road): RoadKind =
  self.kind


func kind* [X: Road](): Lens[X, RoadKind] =
  lens(kind, (self: X, kind: RoadKind) => road(kind, self.name))


func name* [X: Road](): Lens[X, RoadName] =
  lens(name, (self: X, name: RoadName) => road(self.kind, name))
