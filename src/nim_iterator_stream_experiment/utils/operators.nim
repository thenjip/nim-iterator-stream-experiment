proc plus* [T](x, y: T): T =
  x + y


proc minus* [T](x, y: T): T =
  x - y


proc mult* [T](x, y: T): T =
  x * y


proc divInt* [T: SomeInteger](x, y: T): T =
  x div y


proc divFloat* [T: SomeFloat](x, y: T): T =
  x / y


proc modulo* [T: SomeInteger](x, y: T): T =
  x mod y



proc less* [T](x, y: T): bool =
  x < y


proc lessOrEq* [T](x, y: T): bool =
  x <= y


proc equal* [T](x, y: T): bool =
  x == y


proc greaterOrEq* [T](x, y: T): bool =
  x >= y


proc greater* [T](x, y: T): bool =
  x > y
