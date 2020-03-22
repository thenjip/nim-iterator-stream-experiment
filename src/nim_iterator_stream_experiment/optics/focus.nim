import plens
import ../monad/[reader]

import std/[sugar]



type
  Focus* [SR; R; W; SW] = object
    lens: PLens[SR, R, W, SW]
    state: SR



func focusOn* [SR; R; W; SW](
  state: SR;
  lens: PLens[SR, R, W, SW]
): Focus[SR, R, W, SW] =
  Focus[SR, R, W, SW](lens: lens, state: state)



func read* [SR; R; W; SW](self: Focus[SR, R, W, SW]): R =
  self.lens.read().run(self.state)


func write* [SR; R; W; SW](self: Focus[SR, R, W, SW]; value: W): SW =
  self.lens.write(() => value).run(self.state)


func modify* [SR; R; W; SW](self: Focus[SR, R, W, SW]; f: R -> W): SW =
  self.lens.modify(f).run(self.state)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
