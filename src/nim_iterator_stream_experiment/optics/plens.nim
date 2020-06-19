##[
  Polymorphic lenses from functional programming.

  A lens lets one focus on a subpart of a whole (typically a data structure),
  and manipulate that subpart while keeping the rest of the structure.

  Lenses can be seen as the equivalent of properties in object-oriented
  programming. However, lenses can be chained together to let one see further in
  the focused structure.

  Normal lenses let one modify a structure without changing its type.
  Polymorphic lenses allow one to do so while changing its type.
]##



import ../monad/[reader]

import std/[sugar]



type
  MemberReader* [S; T] = Reader[S, T]
  MemberUpdater* [SR; W; SW] = (state: SR, value: W) -> SW

  PLens* [SR; R; W; SW] = object
    reader: MemberReader[SR, R]
    writer: MemberUpdater[SR, W, SW]



func lens* [SR; R; W; SW](
  reader: MemberReader[SR, R];
  writer: MemberUpdater[SR, W, SW]
): PLens[SR, R, W, SW] =
  PLens[SR, R, W, SW](reader: reader, writer: writer)



func read* [SR; R; W; SW](self: PLens[SR, R, W, SW]): Reader[SR, R] =
  self.reader


func write* [SR; R; W; SW](
  self: PLens[SR, R, W, SW];
  value: () -> W
): Reader[SR, SW] =
  (state: SR) => self.writer(state, value())


func modify* [SR; R; W; SW](
  self: PLens[SR, R, W, SW];
  f: R -> W
): Reader[SR, SW] =
  self.read().map(f).flatMap((w: W) => self.write(() => w))



func chain* [SR; R1; W1; SW; R2; W2](
  self: PLens[SR, R1, W1, SW];
  other: PLens[R1, R2, W2, W1]
): PLens[SR, R2, W2, SW] =
  lens(
    self.read().map(other.read()),
    (state: SR, value: W2) =>
      self.modify(other.write(() => value)).run(state)
  )



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
