import ../utils/[chain]

import std/[sugar]



type
  Monad* [A] {.explain.} = concept m, type MA, type MB
    lift(A) is MA
    m.flatMap(A -> MB) is MB

  LeftIdentitySpec* [T; M] = tuple
    initial: T
    f: T -> M

  RightIdentitySpec* [T] = tuple
    expected: T

  AssociativitySpec* [A; B; MA; MB] = tuple
    initial: A
    f: A -> MA
    g: B -> MB

  MonadLawsSpec* [LT; LM; RT; AA; AB; AMA; AMB] = tuple
    leftIdentity: LeftIdentitySpec[LT, LM]
    rightIdentity: RightIdentitySpec[RT]
    associativity: AssociativitySpec[AA, AB, AMA, AMB]



func leftIdentitySpec* [T; M](initial: T; f: T -> M): LeftIdentitySpec[T, M] =
  (initial, f)


func rightIdentitySpec* [T](expected: T): RightIdentitySpec[T] =
  (expected)


func associativitySpec* [A; B; MA; MB](
  initial: A;
  f: A -> MA;
  g: B -> MB;
): AssociativitySpec[A, B, MA, MB] =
  (initial, f, g)



template checkLeftIdentity* [T; M](spec: LeftIdentitySpec[T, M]): bool =
  spec.initial.lift().flatMap(spec.f) == spec.f(spec.initial)



template checkRightIdentity* [T](spec: RightIdentitySpec[T]): bool =
  spec.expected.lift().flatMap(lift) == spec.expected.lift()


template checkAssociativity* [A; B; MA; MB](
  spec: AssociativitySpec[A, B, MA, MB]
): bool =
  spec
    .initial
    .lift()
    .flatMap(spec.f)
    .flatMap(spec.g)
    .`==`(spec.initial.lift().flatMap(spec.f.chain(m => m.flatMap(spec.g))))


template checkMonadLaws* [LT; LM; RT; AA; AB; AMA; AMB](
  spec: MonadLawsSpec[LT, LM, RT, AA, AB, AMA, AMB]
): bool =
  `==`(
    (
      leftIdentity: spec.leftIdentity.checkLeftIdentity(),
      rightIdentity: spec.rightIdentity.checkRightIdentity(),
      associativity: spec.associativity.checkAssociativity()
    ),
    (leftIdentity: true, rightIdentity: true, associativity: true)
  )




when isMainModule:
  import std/[os, unittest]



  suite currentSourcePath().splitFile().name:
    discard
