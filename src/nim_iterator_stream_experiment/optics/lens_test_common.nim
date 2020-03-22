import lens



type
  Street* = object
    name: string
    number: Natural


func street* (name: string; number: Natural): Street =
  Street(name: name, number: number)


func readName* (street: Street): string =
  street.name


func writeName* (street: Street; name: string): Street =
  street(name, street.number)


func readNumber* (street: Street): Natural =
  street.number



func focusOnName* (T: typedesc[Street]): Lens[T, string] =
  lens(readName, writeName)


func focusOnName* [T: Street](): Lens[T, string] =
  T.name()
