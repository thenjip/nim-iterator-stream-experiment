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



proc read* [SR; R; W; SW](self: Focus[SR, R, W, SW]): R =
  self.lens.read().run(self.state)


proc write* [SR; R; W; SW](self: Focus[SR, R, W, SW]; value: W): SW =
  self.lens.write(() => value).run(self.state)


proc modify* [SR; R; W; SW](self: Focus[SR, R, W, SW]; f: R -> W): SW =
  self.lens.modify(f).run(self.state)



proc read* [SR; R; W; SW](state: SR; lens: PLens[SR, R, W, SW]): R =
  state.focusOn(lens).read()


proc write* [SR; R; W; SW](
  state: SR;
  lens: PLens[SR, R, W, SW];
  value: W
): SW =
  state.focusOn(lens).write(value)


proc modify* [SR; R; W; SW](
  state: SR;
  lens: PLens[SR, R, W, SW];
  f: R -> W
): SW =
  state.focusOn(lens).modify(f)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
