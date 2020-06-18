##[
  The `Optional[T]` monad.

  This is another implementation of the `Option`/`Maybe` type, but with only
  pure functional programming.
]##



import identity, predicate
import ../utils/[chain, ifelse]

import std/[sugar]



type
  Nilable* = concept var x
    x = nil

  UnboxError* = object of CatchableError

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
  self.ifSome(f, toNone)


proc map* [A; B](self: Optional[A]; f: A -> B): Optional[B] =
  self.flatMap(f.chain(toSome))



func unbox* [T](self: Optional[T]): T {.raises: [Exception, UnboxError].} =
  self
    .ifSome(
      itself,
      proc (): T =
        raise UnboxError.newException("")
    )



proc filter* [T](self: Optional[T]; predicate: Predicate[T]): Optional[T] =
  self.flatMap(predicate.ifElse(toSome, _ => T.toNone()))



func `==`* [T](self, other: Optional[T]): bool =
  self.ifSome(
    selfValue =>
      other.ifSome(otherValue => selfValue == otherValue, () => false)
    ,
    () => other.isNone()
  )



when isMainModule:
  import monadlaws
  import ../utils/[call, ignore, partialprocs, proctypes, unit]

  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    test """The "Nilable" concept should match standard "nil" types.""":
      proc doTest (T: typedesc) =
        check:
          T is Nilable


      doTest(ptr cstring)
      doTest(pointer)
      doTest(ref tuple[a: Unit; b: char])
      doTest(int -> string)
      doTest(cstring)



    test """The "Nilable" concept should not match standard non "nil" types.""":
      proc doTest (T: typedesc) =
        check:
          T isnot Nilable


      doTest(cuint)
      doTest(string)
      doTest(seq[cdouble])
      doTest(tuple[a: Unit; b: byte])
      doTest(Slice[Natural])
      doTest(Positive)



    test """"nil.toSome()" should raise a "Defect" at runtime.""":
      proc doTest (T: typedesc[Nilable]) =
        expect Defect:
          nil.T.toSome().ignore()


      doTest(pointer)



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
              expected =>
                rightIdentitySpec(expected, _ => expected.typeof().toNone())
            )
          ,
          associativitySpec(
            69,
            toSome,
            i => toSome(toNone[i.typeof()]),
            (p: toNone[int].typeof()) => p.call().`$`().toSome()
          )
        )
      )



    test """Computations that use "Optional[T]" and without side effects should be compatible with compile time execution.""":
      template doTest [A; B](
        sut: static[proc (argument: Optional[A]): B {.noSideEffect.}];
        argument: Optional[A]{noSideEffect};
        expected: static B
      ): proc () {.nimcall.} =
        (
          proc () =
            const actual = sut.call(argument)

            check:
              actual == expected
        )


      func sut1 [T](arg: Optional[T]): T =
        arg.unbox()

      func expected1 (): int =
        -2

      func argument1 (): Optional[expected1.returnType()] =
        expected1().toSome()


      func sut2 [T: Nilable](arg: Optional[T]): T =
        arg.ifSome(itself, () => nil.T)

      func expected2 (): ref char =
        nil

      func argument2 (): Optional[expected2.returnType()] =
        expected2.returnType().toNone()


      for t in [
        doTest(sut1[expected1.returnType()], argument1(), expected1()),
        doTest(sut2[expected2.returnType()], argument2(), expected2())
      ]:
        t.call()



    test """"unbox" with "some" should return the "expected" value.""":
      proc doTest [T](expected: T) =
        let actual = expected.toSome().unbox()

        check:
          actual == expected


      doTest(["a", "b"])
      doTest(1)
      doTest((1, 1.005))
      doTest(() => 5)



    test """"unbox" with "none" should raise a "CatchableError" at runtime.""":
      proc doTest (T: typedesc) =
        expect CatchableError:
          T.toNone().unbox().ignore()


      doTest(int)



    test """"self.filter(predicate)" with "none" should return "none".""":
      proc doTest [T](predicate: Predicate[T]) =
        let
          expected = T.toNone()
          actual = expected.filter(predicate)

        check:
          actual == expected


      doTest((s: string) => s.len() > 0)
      doTest(alwaysTrue[uint])
      doTest(alwaysFalse[ptr cint])



    test """"self.filter(predicate)" with "some" and a predicate that will be verified should return "self".""":
      proc doTest [T](value: T) =
        let
          expected = value.toSome()
          actual = expected.filter(alwaysTrue[T])

        check:
          actual == expected


      doTest(new cfloat)
      doTest('a')



    test """"self.filter(predicate)" with "some" and a predicate that will not be verified should return "none".""":
      proc doTest [T](value: T) =
        let
          expected = T.toNone()
          actual = value.toSome().filter(alwaysFalse[T])

        check:
          actual == expected


      doTest("abc".cstring)
      doTest(partial($ ?:uint))
      doTest(0)
