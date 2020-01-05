import chain, identity, utils

import std/[sugar]



type
  SomePointer* = ref or ptr or pointer or cstring or proc

  Optional* [T] = object
    when T is SomePointer:
      value: T
    else:
      case empty: bool
        of true:
          discard
        of false:
          value: T



func toNone* (T: typedesc[SomePointer]): Optional[T] =
  Optional[T](value: nil)


func toNone* (T: typedesc): Optional[T] =
  Optional[T](empty: true)


func toNone* [T](): Optional[T] =
  T.toNone()


func toSome* [T: SomePointer](value: T): Optional[T] =
  assert(not value.isNil())

  Optional[T](value: value)


func toSome* [T: not SomePointer](value: T): Optional[T] =
  Optional[T](empty: false, value: value)



func isNone* [T: SomePointer](self: Optional[T]): bool =
  self.value.isNil()


func isNone* [T: not SomePointer](self: Optional[T]): bool =
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
  import io, unit

  import std/[os, unittest]



  template lazyCheck (checks: untyped): proc (): Unit =
    (
      proc (): Unit =
        check:
          checks
    )



  suite currentSourcePath().splitFile().name:
    test "toSome: not SomePointer":
      proc toSomeNotSomePtrTest [T: not SomePointer](expectedVal: T) =
        let
          expected = Optional[T](empty: false, value: expectedVal)
          sut = expectedVal.toSome()

        check:
          sut == expected


      toSomeNotSomePtrTest(1)
      toSomeNotSomePtrTest(@[0])
      toSomeNotSomePtrTest("a")



    test "toSome: SomePointer":
      proc toSomeSomePtrTest [T: SomePointer](expectedVal: T) =
        let
          expected = Optional[T](value: expectedVal)
          sut = expectedVal.toSome()

        check:
          sut == expected


      toSomeSomePtrTest("a".cstring)
      toSomeSomePtrTest(() => -1)
      toSomeSomePtrTest(ArithmeticError.newException(""))



    test "toNone: not SomePointer":
      proc toNoneNotSomePtrTest (T: typedesc[not SomePointer]) =
        let
          expected = Optional[T](empty: true)
          sut = T.toNone()

        check:
          sut == expected


      toNoneNotSomePtrTest(set[char])
      toNoneNotSomePtrTest({'a': 0}.typeof())



    test "toNone: SomePointer":
      proc toNoneSomePtrTest (T: typedesc[SomePointer]) =
        let
          expected = Optional[T](value: nil)
          sut = T.toNone()

        check:
          sut == expected


      toNoneSomePtrTest(pointer)
      toNoneSomePtrTest(ref RootObj)
      toNoneSomePtrTest(() -> void)



    test "ifSome: with some":
      proc ifSomeWithSomeTest [A; B](initialVal: A; expected, unexpected: B) =
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


      ifSomeWithSomeTest(['a'], 0, 1)
      ifSomeWithSomeTest("a", () => 0, () => 0)



    test "ifSome: with none":
      proc ifSomeWithNoneTest [B](A: typedesc; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut = A.toNone().ifSome(_ => unexpected, () => expected)

        check:
          sut == expected


      ifSomeWithNoneTest(uint16, "a", "b")
      ifSomeWithNoneTest(seq[int], false, true)



    test "ifNone: with some":
      proc ifNoneWithSomeTest [A; B](initialVal: A; expected, unexpected: B) =
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


      ifNoneWithSomeTest("a", 'a', 'b')



    test "ifNone: with none":
      proc ifNoneWithNoneTest [B](A: typedesc; expected, unexpected: B) =
        require:
          expected != unexpected

        let sut = A.toNone().ifNone(() => expected, _ => unexpected)

        check:
          sut == expected


      ifNoneWithNoneTest(cfloat, -1, 6)



    test "get: with some":
      proc getWithSomeTest [T](expected: T) =
        let sut = expected.toSome().get()

        check:
          sut == expected


      getWithSomeTest(["a", "b"])



    test "get: with none":
      proc getWithNoneTest (T: typedesc) =
        expect ValueError:
          T.toNone().get().ignore()


      getWithNoneTest(int)



    test "flatMap: with some":
      proc flatMapWithSomeTest [A; B](initialVal: A; expectedVal: B) =
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


      flatMapWithSomeTest(0, 'a')



    test "flatMap: with none":
      proc flatMapWithNoneTest [B](A: typedesc; unexpectedVal: B) =
        let
          expected = B.toNone()
          sut = A.toNone().flatMap((_: A) => unexpectedVal.toSome())

        check:
          sut == expected


      flatMapWithNoneTest(string, () => 0.0)



    test "map: with some":
      proc mapWithSomeTest [A; B](initialVal: A; expectedVal: B) =
        let
          expected = expectedVal.toSome()
          sut =
            initialVal
            .toSome()
            .map(a => lazyCheck(a == initialVal).chain(_ => expectedVal).run())

        check:
          sut == expected


      mapWithSomeTest(2.toBiggestFloat(), {0})



    test "map: with none":
      proc mapWithNoneTest [B](A: typedesc; unexpectedVal: B) =
        let
          expected = B.toNone()
          sut = A.toNone().map(_ => unexpectedVal)

        check:
          sut == expected


      mapWithNoneTest(float32, 'a')



    test "filter: some -> some":
      proc filterSomeSomeTest [T](val: T; predicate: T -> bool) =
        require:
          val.predicate()

        let
          expected = val.toSome()
          sut = val.toSome().filter(predicate)

        check:
          sut == expected


      filterSomeSomeTest(86, i => i > 80)



    test "filter: some -> none":
      proc filterSomeNoneTest [T](val: T; predicate: T -> bool) =
        require:
          not val.predicate()

        let
          expected = T.toNone()
          sut = val.toSome().filter(predicate)

        check:
          sut == expected


        filterSomeNoneTest("", s => s.len() > 0)



    test "filter: none -> none":
      proc filterNoneNoneTest (T: typedesc; predicate: T -> bool) =
        let
          expected = T.toNone()
          sut = T.toNone().filter(predicate)

        check:
          sut == expected


      filterNoneNoneTest(char, c => true)
