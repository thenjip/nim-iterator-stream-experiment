import std/[macros, sugar]



proc call* [T](p: () -> T): T =
  p()


macro call* (p: proc; arg1: untyped; remaining: varargs[untyped]): untyped =
  result = newCall(p, arg1)

  for arg in remaining:
    result.add(arg)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "call: 0 arguments":
      proc doTest [T](sut: () -> T) =
        check:
          sut.call() == sut()


      doTest(() => 156)
      doTest(() => "a")



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
