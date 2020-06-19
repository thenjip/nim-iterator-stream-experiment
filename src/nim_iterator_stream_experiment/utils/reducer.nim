import call

import std/[sugar]



type
  Reducer* [R; T] = (R, T) -> R



proc run* [R; T](self: Reducer[R, T]; reduction: R; item: T): R =
  self.call(reduction, item)



func map* [R; T](self: Reducer[R, T]; f: R -> R): Reducer[R, T] =
  (reduction: R, item: T) => self.run(reduction, item).f()
