##[
  The `Optional[T]` monad.

  This is another implementation of the `Option`/`Maybe` type, but with only
  pure functional programming.
]##



import identity
import ../utils/[chain, ifelse]

import std/[sugar]



type
  Nilable* = concept var x
    x = nil

  Optional* [T] = object
    when T is Nilable:
      value: T
    else:
      case empty: bool
        of true:
          discard
        else:
          value: T



func toNone* (T: typedesc[Nilable]): Optional[T] =
  Optional[T](value: nil)


func toNone* (T: typedesc[not Nilable]): Optional[T] =
  Optional[T](empty: true)


func toNone* [T](): Optional[T] =
  T.toNone()



func toSome* [T: Nilable](value: T): Optional[T] =
  assert(not value.isNil())

  Optional[T](value: value)


func toSome* [T: not Nilable](value: T): Optional[T] =
  Optional[T](empty: false, value: value)



func isNone* [T: Nilable](self: Optional[T]): bool =
  self.value.isNil()


func isNone* [T: not Nilable](self: Optional[T]): bool =
  self.empty



func isSome* [T](self: Optional[T]): bool =
  not self.isNone()



proc ifNone* [A; B](self: Optional[A]; then: () -> B; `else`: A -> B): B =
  self.isNone().ifElse(then, () => self.value.`else`())


proc ifSome* [A; B](self: Optional[A]; then: A -> B; `else`: () -> B): B =
  self.isSome().ifElse(() => self.value.then(), `else`)



proc flatMap* [A; B](self: Optional[A]; f: A -> Optional[B]): Optional[B] =
  self.ifSome(f, toNone[B])


proc map* [A; B](self: Optional[A]; f: A -> B): Optional[B] =
  self.flatMap(f.chain(toSome))



func get* [T](self: Optional[T]): T {.raises: [Exception, ValueError].} =
  self.ifSome(itself, proc (): T = raise ValueError.newException(""))



proc filter* [T](self: Optional[T]; predicate: T -> bool): Optional[T] =
  self.flatMap(
    (item: T) => item.predicate().ifElse(() => item.toSome(), toNone[T])
  )



func `==`* [T](self, other: Optional[T]): bool =
  self.ifSome(
    selfValue =>
      other.ifSome(otherValue => selfValue == otherValue, () => false)
    ,
    () => other.isNone()
  )



when isMainModule:
  import io, monadlaws
  import ../utils/[ignore, unit]

  import std/[os, unittest]



  template lazyCheck (checks: untyped): () -> Unit =
    (
      proc (): Unit =
        check:
          checks
    )



  suite currentSourcePath().splitFile().name:
    test "toSome: not Nilable":
      proc doTest [T: not Nilable](expectedVal: T) =
        let
          expected = Optional[T](empty: false, value: expectedVal)
          sut = expectedVal.toSome()

        check:
          sut == expected


      doTest(1)
      doTest(@[0])
      doTest("a")



    test "toSome: Nilable":
      proc doTest [T: Nilable](expectedVal: T) =
        let
          expected = Optional[T](value: expectedVal)
          sut = expectedVal.toSome()

        check:
          sut == expected


      doTest("a".cstring)
      doTest(() => -1)
      doTest(ArithmeticError.newException(""))



    test "toNone: not Nilable":
      proc doTest (T: typedesc[not Nilable]) =
        let
          expected = Optional[T](empty: true)
          sut = T.toNone()

        check:
          sut == expected


      doTest(set[char])
      doTest({'a': 0}.typeof())



    test "toNone: Nilable":
      proc doTest (T: typedesc[Nilable]) =
        let
          expected = Optional[T](value: nil)
          sut = T.toNone()

        check:
          sut == expected


      doTest(pointer)
      doTest(ref RootObj)
      doTest(() -> void)



    test """"Optional[T]" should obey the monad laws.""":
      proc doTest [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC](
        spec: MonadLawsSpec[LA, LMA, LMB, RT, RM, AA, AB, AMA, AMB, AMC]
      ) =
        check:
          spec.checkMonadLaws()


      doTest(
        monadLawsSpec(
          leftIdentitySpec(NaN, toSome, _ => float32.toNone()),
          ["".cstring, nil]
            .apply(
              initial =>
                rightIdentitySpec(initial, _ => initial.typeof().toNone())
            )
          ,
          associativitySpec(
            69,
            toSome,
            (i) => toSome(i - 5),
            (i: int) => i.`$`().toSome()
          )
        )
      )



    test "ifSome: with some":
      proc doTest [A; B](initialVal: A; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut =
          initialVal
            .toSome()
            .ifSome(
              a => lazyCheck(a == initialVal).chain(_ => expected).run(),
              () => unexpected
            )

        check:
          sut == expected


      doTest(['a'], 0, 1)
      doTest("a", () => 0, () => 0)



    test "ifSome: with none":
      proc doTest [B](A: typedesc; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut = A.toNone().ifSome(_ => unexpected, () => expected)

        check:
          sut == expected


      doTest(uint16, "a", "b")
      doTest(seq[int], false, true)



    test "ifNone: with some":
      proc doTest [A; B](initialVal: A; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut =
          initialVal
          .toSome()
          .ifNone(
            () => unexpected,
            a => lazyCheck(a == initialVal).chain(_ => expected).run()
          )

        check:
          sut == expected


      doTest("a", 'a', 'b')



    test "ifNone: with none":
      proc doTest [B](A: typedesc; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut = A.toNone().ifNone(() => expected, _ => unexpected)

        check:
          sut == expected


      doTest(cfloat, -1, 6)



    test "get: with some":
      proc doTest [T](expected: T) =
        let sut = expected.toSome().get()

        check:
          sut == expected


      doTest(["a", "b"])



    test "get: with none":
      proc doTest (T: typedesc) =
        expect ValueError:
          T.toNone().get().ignore()


      doTest(int)



    test "flatMap: with some":
      proc doTest [A; B](initialVal: A; expectedVal: B) =
        let
          expected = expectedVal.toSome()
          sut =
            initialVal
            .toSome()
            .flatMap(
              (a: A) =>
                lazyCheck(a == initialVal)
                .chain(_ => expectedVal.toSome())
                .run()
            )

        check:
          sut == expected


      doTest(0, 'a')



    test "flatMap: with none":
      proc doTest [B](A: typedesc; unexpectedVal: B) =
        let
          expected = B.toNone()
          sut = A.toNone().flatMap((_: A) => unexpectedVal.toSome())

        check:
          sut == expected


      doTest(string, () => 0.0)



    test "map: with some":
      proc doTest [A; B](initialVal: A; expectedVal: B) =
        let
          expected = expectedVal.toSome()
          sut =
            initialVal
            .toSome()
            .map(a => lazyCheck(a == initialVal).chain(_ => expectedVal).run())

        check:
          sut == expected


      doTest(2.toBiggestFloat(), {0})



    test "map: with none":
      proc doTest [B](A: typedesc; unexpectedVal: B) =
        let
          expected = B.toNone()
          sut = A.toNone().map(_ => unexpectedVal)

        check:
          sut == expected


      doTest(float32, 'a')



    test "filter: some -> some":
      proc doTest [T](val: T; predicate: T -> bool) =
        require:
          val.predicate()

        let
          expected = val.toSome()
          sut = val.toSome().filter(predicate)

        check:
          sut == expected


      doTest(86, i => i > 80)



    test "filter: some -> none":
      proc doTest [T](val: T; predicate: T -> bool) =
        require:
          not val.predicate()

        let
          expected = T.toNone()
          sut = val.toSome().filter(predicate)

        check:
          sut == expected


        doTest("", s => s.len() > 0)



    test "filter: none -> none":
      proc doTest (T: typedesc; predicate: T -> bool) =
        let
          expected = T.toNone()
          sut = T.toNone().filter(predicate)

        check:
          sut == expected


      doTest(char, c => true)
