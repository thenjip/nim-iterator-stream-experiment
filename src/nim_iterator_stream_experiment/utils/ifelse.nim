import std/[sugar]



proc ifElse* [T](condition: bool; then, `else`: () -> T): T =
  if condition:
    then()
  else:
    `else`()



when isMainModule:
  import chain, ignore, operators, unit, variables
  import ../monad/[identity]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """"condition.ifElse(then, `else`)" should enter only 1 of the 2 paths.""":
      proc doTest [T](condition: bool; then, `else`: () -> T) =
        const expected = 1.Natural

        var pathTaken = 0.Natural

        let incPathCounter =
          (value: T) => pathTaken.modify(plus1).apply(_ => value)

        condition
          .ifElse(then.chain(incPathCounter), `else`.chain(incPathCounter))
          .ignore()

        let actual = pathTaken.read()

        check:
          actual == expected


      doTest(false, () => unit(), () => unit())
      doTest(true, () => -5, () => int.high())
