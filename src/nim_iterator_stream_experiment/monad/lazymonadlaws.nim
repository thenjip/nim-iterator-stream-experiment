##[
  Utilities to check whether a type `M` obeys the monad laws.

  A lazy monad builds computations lazily. The user decides when to run them.

  `M` must have:
    - A ``flatMap[A; B]`` procedure with the signature
      ``(M[A], A -> M[B]) -> M[B]``.
    - A ``run[A; R]`` procedure with the signature ``(M[A], R) -> A``.
]##



import ../utils/[chain]

import std/[sugar]



type
  LazyMonad* [T; R] = concept m
    m.run(R) is T

  LeftIdentitySpec* [A; MA; MB; R] = tuple
    initial: A
    lift: A -> MA
    f: A -> MB
    runArg: R

  RightIdentitySpec* [T; M; R] = tuple
    expected: T
    lift: T -> M
    runArg: R

  AssociativitySpec* [A; B; MA; MB; MC; R] = tuple
    initial: A
    lift: A -> MA
    f: A -> MB
    g: B -> MC
    runArg: R

  MonadLawsSpec* [LA; LMA; LMB; LR; RT; RM; RR; AA; AB; AMA; AMB; AMC; AR] =
    tuple
      leftIdentity: LeftIdentitySpec[LA, LMA, LMB, LR]
      rightIdentity: RightIdentitySpec[RT, RM, RR]
      associativity: AssociativitySpec[AA, AB, AMA, AMB, AMC, AR]



func leftIdentitySpec* [A; MA; MB; R](
  initial: A;
  lift: A -> MA;
  f: A -> MB;
  runArg: R
): LeftIdentitySpec[A, MA, MB, R] =
  (initial, lift, f, runArg)


func rightIdentitySpec* [T; M; R](
  expected: T;
  lift: T -> M;
  runArg: R
): RightIdentitySpec[T, M, R] =
  (expected, lift, runArg)


func associativitySpec* [A; B; MA; MB; MC; R](
  initial: A;
  lift: A -> MA;
  f: A -> MB;
  g: B -> MC;
  runArg: R
): AssociativitySpec[A, B, MA, MB, MC, R] =
  (initial, lift, f, g, runArg)


func monadLawsSpec* [LA; LMA; LMB; LR; RT; RM; RR; AA; AB; AMA; AMB; AMC; AR](
  leftIdentity: LeftIdentitySpec[LA, LMA, LMB, LR];
  rightIdentity: RightIdentitySpec[RT, RM, RR];
  associativity: AssociativitySpec[AA, AB, AMA, AMB, AMC, AR]
): MonadLawsSpec[LA, LMA, LMB, LR, RT, RM, RR, AA, AB, AMA, AMB, AMC, AR] =
  (leftIdentity, rightIdentity, associativity)



template checkLeftIdentity* [A; MA; MB; R](
  spec: LeftIdentitySpec[A, MA, MB, R]
): bool =
  spec
    .lift(spec.initial)
    .flatMap(spec.f)
    .run(spec.runArg)
    .`==`(spec.f(spec.initial).run(spec.runArg))



template checkRightIdentity* [T; M; R](spec: RightIdentitySpec[T, M, R]): bool =
  spec
    .lift(spec.expected)
    .flatMap(spec.lift)
    .run(spec.runArg)
    .`==`(spec.lift(spec.expected).run(spec.runArg))


template checkAssociativity* [A; B; MA; MB; MC; R](
  spec: AssociativitySpec[A, B, MA, MB, MC, R]
): bool =
  spec
    .lift(spec.initial)
    .flatMap(spec.f)
    .flatMap(spec.g)
    .run(spec.runArg)
    .`==`(
      spec
        .lift(spec.initial)
        .flatMap(spec.f.chain(m => m.flatMap(spec.g)))
        .run(spec.runArg)
    )


template checkMonadLaws* [LA; LMA; LMB; LR; RT; RM; RR; AA; AB; AMA; AMB; AMC; AR](
  spec: MonadLawsSpec[LA, LMA, LMB, LR, RT, RM, RR, AA, AB, AMA, AMB, AMC, AR]
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
