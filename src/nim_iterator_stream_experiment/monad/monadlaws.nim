##[
  Utilities to check whether a type `M` obeys the monad laws.

  `M` must have a ``flatMap[A; B]`` procedure defined with the following
  signature: ``(M[A], A -> M[B]) -> M[B]``.

  Concepts cannot be currently used to implement the monad concept in Nim.
  See this related `issue <https://github.com/nim-lang/Nim/issues/5650>`_ .
]##



import ../utils/[chain]

import std/[sugar]



type
  LeftIdentitySpec* [A; MA; MB] = tuple
    initial: A
    lift: A -> MA
    f: A -> MB

  RightIdentitySpec* [T; M] = tuple
    expected: T
    lift: T -> M

  AssociativitySpec* [A; B; MA; MB; MC] = tuple
    initial: A
    lift: A -> MA
    f: A -> MB
    g: B -> MC

  MonadLawsSpec* [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC] = tuple
    leftIdentity: LeftIdentitySpec[LA, LMA, LMB]
    rightIdentity: RightIdentitySpec[RT, RM]
    associativity: AssociativitySpec[AA, AB, AMA, AMB, AMC]



func leftIdentitySpec* [A; MA; MB](
  initial: A;
  lift: A -> MA;
  f: A -> MB
): LeftIdentitySpec[A, MA, MB] =
  (initial, lift, f)


func rightIdentitySpec* [T; M](
  expected: T;
  lift: T -> M
): RightIdentitySpec[T, M] =
  (expected, lift)


func associativitySpec* [A; B; MA; MB; MC](
  initial: A;
  lift: A -> MA;
  f: A -> MB;
  g: B -> MC;
): AssociativitySpec[A, B, MA, MB, MC] =
  (initial, lift, f, g)



func monadLawsSpec* [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC](
  leftIdentity: LeftIdentitySpec[LA, LMA, LMB];
  rightIdentity: RightIdentitySpec[RT, RM];
  associativity: AssociativitySpec[AA, AB, AMA, AMB, AMC]
): MonadLawsSpec[LA, LMA, LMB, RT, RM, AA, AB, AMA, AMB, AMC] =
  (leftIdentity, rightIdentity, associativity)



template checkLeftIdentity* [A; MA; MB](
  spec: LeftIdentitySpec[A, MA, MB]
): bool =
  spec.lift(spec.initial).flatMap(spec.f) == spec.f(spec.initial)



template checkRightIdentity* [T; M](spec: RightIdentitySpec[T, M]): bool =
  spec.lift(spec.expected).flatMap(spec.lift) == spec.lift(spec.expected)


template checkAssociativity* [A; B; MA; MB; MC](
  spec: AssociativitySpec[A, B, MA, MB, MC]
): bool =
  spec
    .lift(spec.initial)
    .flatMap(spec.f)
    .flatMap(spec.g)
    .`==`(spec.lift(spec.initial).flatMap(spec.f.chain(m => m.flatMap(spec.g))))


template checkMonadLaws* [LA; LMA; LMB; RT; RM; AA; AB; AMA; AMB; AMC](
  spec: MonadLawsSpec[LA, LMA, LMB, RT, RM, AA, AB, AMA, AMB, AMC]
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
