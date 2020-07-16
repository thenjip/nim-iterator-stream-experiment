import ../../../plens

import std/[sugar]



type
  BlockBody* [T] = () -> T
  BlockLabel* = string

  Block* [T] = object
    label: BlockLabel
    body: BlockBody[T]



func `block`* [T](label: string; body: BlockBody[T]): Block[T] =
  Block[T](label: label, body: body)


func label* [T](self: Block[T]): BlockLabel =
  self.label


func body* [T](self: Block[T]): BlockBody[T] =
  self.body


func label* [T](X: typedesc[Block[T]]): PLens[X, BlockLabel, BlockLabel, X] =
  lens(label[T], (self: X, label: BlockLabel) => `block`(label, self.body))


func body* [A](
  X: typedesc[Block[A]];
  B: typedesc
): PLens[X, BlockBody[A], BlockBody[B], Block[B]] =
  lens(body[A], (self: X, body: BlockBody[B]) => `block`(self.label, body))
