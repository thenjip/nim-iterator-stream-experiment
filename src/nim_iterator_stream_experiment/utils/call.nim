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
  import operators

  import std/[os, sugar, unittest]



  suite currentSourcePath().splitFile().name:
    test """"f.call(arg)" should produce "f(arg)".""":
      proc doTest [A; B](f: A -> B; arg: A) =
        let
          actual = f.call(arg)
          expected = f(arg)

        check:
          actual == expected


      doTest((c: char) => c.byte, 'a')
      doTest((s: seq[string]) => s.len(), @["a", "b"])



    test """"f.call(arg1, arg2)" should produce "f(arg1, arg2)".""":
      proc doTest [A; B; C](f: (A, B) -> C; arg1: A; arg2: B) =
        let
          actual = f.call(arg1, arg2)
          expected = f(arg1, arg2)

        check:
          actual == expected


      doTest(plus[int], 1, 2)
