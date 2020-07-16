import "block"
import ../../../plens

import std/[sugar]



type
  Condition* = () -> bool
  ThenBlock* [T] = Block[T]

  ConditionalBlock* [T] = object
    condition: Condition
    then: ThenBlock[T]



func conditionalBlock* [T](
  condition: Condition;
  then: ThenBlock[T]
): ConditionalBlock[T] =
  ConditionalBlock[T](condition: condition, then: then)


func condition* [T](self: ConditionalBlock[T]): Condition =
  self.condition


func thenBlock* [T](self: ConditionalBlock[T]): ThenBlock[T] =
  self.then


func thenLabel* [T](self: ConditionalBlock[T]): BlockLabel =
  self.thenBlock().label()


func thenBody* [T](self: ConditionalBlock[T]): BlockBody[T] =
  self.thenBlock().body()



func condition* [T](
  X: typedesc[ConditionalBlock[T]]
): PLens[X, Condition, Condition, X] =
  lens(
    condition[T],
    (self: X, condition: Condition) =>
      conditionalBlock(condition, self.then)
  )


func thenBlock* [A](
  X: typedesc[ConditionalBlock[A]];
  B: typedesc
): PLens[X, ThenBlock[A], ThenBlock[B], ConditionalBlock[B]] =
  lens(
    thenBlock[A],
    (self: X, then: ThenBlock[B]) => conditionalBlock(self.condition, then)
  )


func thenLabel* [T](
  X: typedesc[ConditionalBlock[T]]
): PLens[X, BlockLabel, BlockLabel, X] =
  X.thenBlock(T).chain(ThenBlock[T].label())


func thenBody* [A](
  X: typedesc[ConditionalBlock[A]];
  B: typedesc
): PLens[X, BlockBody[A], BlockBody[B], ConditionalBlock[B]] =
  X.thenBlock(B).chain(ThenBlock[A].body(B))
