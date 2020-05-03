import std/[macros]



macro returnType* (T: typedesc[proc]): typedesc =
  T.getType()[1][1].getTypeInst()


template returnType* (p: proc): typedesc =
  p.typeof().returnType()



when isMainModule:
  import std/[os, sugar, unittest]



  suite currentSourcePath().splitFile().name:
    test """"T.returnType()" should return the return type of the given procedure type.""":
      template doTest (T: typedesc[proc]; expected: typedesc) =
        check:
          T.returnType().`is`(expected)


      type
        SomeProc = ref NilAccessError -> seq[char]
        SomeGenericProc [T; R] = T -> R


      doTest(proc (), void)
      doTest(
        proc (a: string): Natural {.cdecl.},
        range[Natural.low() .. Natural.high()]
      )
      doTest((int, () -> tuple[]) -> pointer, pointer)
      doTest(SomeProc, seq[char])
      doTest(
        SomeGenericProc[float, ref FloatingPointError],
        ref FloatingPointError
      )
      doTest(() -> var int, var int)



    test """"p.returnType()" should return the return type of the given procedure instance.""":
      template doTest (p: proc; expected: typedesc) =
        check:
          p.returnType().`is`(expected)


      func someProc (): Positive {.closure.} =
        1


      doTest(stackTraceAvailable, bool)
      doTest(absolutePath, string)
      doTest(someProc, Positive)
