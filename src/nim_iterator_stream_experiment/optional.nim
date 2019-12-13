import utils

import std/[sugar]



type Optional* [T] = object
  case empty: bool
    of true:
      discard
    of false:
      value: T



func toNone* (T: typedesc): Optional[T] =
  Optional[T](empty: true)


func toNone* [T](): Optional[T] =
  T.toNone()


proc toSome* [T](value: T): Optional[T] =
  Optional[T](empty: false, value: value)


func isNone* [T](self: Optional[T]): bool =
  self.empty


func isSome* [T](self: Optional[T]): bool =
  not self.isNone()


proc ifSome* [A; B](self: Optional[A]; then: A -> B; `else`: () -> B): B =
  self.isSome().ifElse(
    () => self.value.then(),
    `else`
  )


proc flatMap* [A; B](self: Optional[A]; f: A -> Optional[B]): Optional[B] =
  self.ifSome(f, toNone[B])


proc map* [A; B](self: Optional[A]; f: A -> B): Optional[B] =
  self.flatMap((value: A) => value.f().toSome())


proc get* [T](self: Optional[T]): T {.raises: [Exception, ValueError].} =
  self.ifSome(value => value, proc (): T = raise ValueError.newException(""))


proc filter* [T](self: Optional[T]; predicate: T -> bool): Optional[T] =
  self.flatMap(
    (item: T) => item.predicate().ifElse(() => item.toSome(), toNone[T])
  )


proc `==`* [T](self, other: Optional[T]): bool =
  self.ifSome(
    selfValue =>
      other.ifSome(otherValue => selfValue == otherValue, () => false)
    ,
    () => other.isNone()
  )
