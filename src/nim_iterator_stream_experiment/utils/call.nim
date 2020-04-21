import std/[macros, sugar]



template call* [T](p: () -> T): T =
  p()


macro call* (p: proc; arg1: untyped; remaining: varargs[untyped]): untyped =
  result = newCall(p, arg1)

  for arg in remaining:
    result.add(arg)



when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test "call: 0 arguments":
      proc doTest [T](expected: T) =
        let sut = () => expected

        check:
          sut.call() == expected


      doTest(156)
      doTest("a")



    test "call: 1 argument":
      proc doTest [T](expected: T) =
        let sut = (a: T) => a

        check:
          sut.call(expected) == expected


      doTest('a')
      doTest(["a", "b"])



    test "call: 2 arguments":
      proc doTest [A; B; C](sut: (A, B) -> C; arg1: A; arg2: B; expected: C) =
        check:
          sut.call(arg1, arg2) == expected


      proc plus [I: SomeInteger](a, b: I): I =
        a + b


      doTest(plus[int], 1, 2, 1.plus(2))
