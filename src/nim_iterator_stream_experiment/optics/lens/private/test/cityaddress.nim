import road
import ../../../lens

import std/[sugar]



type
  HouseNumber* = Natural

  CityAddress* = object
    number: HouseNumber
    road: Road



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
