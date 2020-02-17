import std/[sugar]



template lambda* (`expr`: untyped): untyped =
  () => `expr`



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "lambda: unified function call syntax":
      check:
        compiles:
          lambda 0
        compiles:
          0.lambda()
        compiles:
          lambda(0)
