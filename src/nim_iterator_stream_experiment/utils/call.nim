import std/[macros]



proc call* [T](p: proc (): T {.nimcall.}): T =
  p()


proc call* [T](p: proc (): T {.closure.}): T =
  p()


proc call* (p: proc () {.nimcall.}) =
  p()


proc call* (p: proc () {.closure.}) =
  p()


macro call* (p: proc; arg1: untyped; remaining: varargs[untyped]): untyped =
  result = newCall(p, arg1)

  for arg in remaining:
    result.add(arg)



when isMainModule:
  import std/[os, sugar, unittest]



  suite currentSourcePath().splitFile().name:
    test "call: 1 argument":
      proc doTest [A; B](sut: A -> B; arg: A) =
        check:
          sut.call(arg) == sut(arg)


      doTest((c: char) => c.byte, 'a')
      doTest((s: seq[string]) => s, @["a", "b"])



    test "call: 2 arguments":
      proc doTest [A; B; C](sut: (A, B) -> C; arg1: A; arg2: B) =
        check:
          sut.call(arg1, arg2) == sut(arg1, arg2)


      proc plus [I: SomeInteger](a, b: I): I =
        a + b


      doTest(plus[int], 1, 2)
