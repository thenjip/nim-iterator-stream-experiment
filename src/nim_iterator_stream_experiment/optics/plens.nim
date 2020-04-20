import ../monad/[reader]

import std/[sugar]



type
  PLensReader* [S; T] = Reader[S, T]
  PLensWriter* [SR; W; SW] = (state: SR, value: W) -> SW

  PLens* [SR; R; W; SW] = object
    reader: PLensReader[SR, R]
    writer: PLensWriter[SR, W, SW]



func lens* [SR; R; W; SW](
  reader: PLensReader[SR, R];
  writer: PLensWriter[SR, W, SW]
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
