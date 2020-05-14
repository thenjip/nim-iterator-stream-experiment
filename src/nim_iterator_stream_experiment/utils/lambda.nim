import std/[sugar]



template lambda* [T](expr: T): proc =
  () => expr



when isMainModule:
  import call

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"expr.lambda().call().typeof()" should be "expr.typeof()".""":
      template doTest [T](expr: T): proc () {.nimcall.} =
        (
          proc () =
            check:
              expr.lambda().call().typeof().`is`(T)
        )


      for t in [doTest(1), doTest("abc"), doTest(new char)]:
        t.call()
