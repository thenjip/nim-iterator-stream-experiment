import ../lens

import std/[sugar]



type
  HouseNumber* = Natural
  RoadName* = string

  RoadKind* {.pure.} = enum
    Street
    Avenue
    Boulevard

  Road* = object
    kind: RoadKind
    name: RoadName

  CityAddress* = object
    number: HouseNumber
    road: Road



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



func cityAddress* (number: Natural; road: Road): CityAddress =
  CityAddress(number: number, road: road)


func number* (self: CityAddress): HouseNumber =
  self.number


func road* (self: CityAddress): Road =
  self.road


func number* [X: CityAddress](): Lens[X, HouseNumber] =
  lens(
    (self: X) => self.number,
    (self: X, number: HouseNumber) => cityAddress(number, self.road)
  )


func road* [X: CityAddress](): Lens[X, Road] =
  lens(
    (self: X) => self.road,
    (self: X, road: Road) => cityAddress(self.number, road)
  )


func roadKind* (X: typedesc[CityAddress]): Lens[X, RoadKind] =
  road[CityAddress]().chain(kind[Road]())


func roadName* (X: typedesc[CityAddress]): Lens[X, RoadName] =
  road[CityAddress]().chain(name[Road]())
