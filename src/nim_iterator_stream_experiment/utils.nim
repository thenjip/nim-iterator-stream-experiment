import std/[sugar]



func ignore* [T](_: T) =
  discard


func default* [T](): T =
  T.default()


proc apply* [A; B](a: A; f: A -> B): B =
  a.f()


proc ifElse* [T](condition: bool; then: () -> T; `else`: () -> T): T =
  if condition:
    then()
  else:
    `else`()



when isMainModule:
  discard
