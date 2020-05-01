from monadlaws import Monad
import ../utils/[chain]

import std/[sugar]




type
  LazyMonad* [T; R] {.explain.} = concept m, type M of Monad[T]
    m.run(R) is T

  LeftIdentitySpec* [T; M; R] = tuple
    initial: T
    f: T -> M
    runArg: R

  RightIdentitySpec* [T; R] = tuple
    expected: T
    runArg: R

  AssociativitySpec* [A; B; MA; MB; R] = tuple
    initial: A
    f: A -> MA
    g: B -> MB
    runArg: R

  MonadLawsSpec* [LT; LM; LR; RT; RR; AA; AB; AMA; AMB; AR] = tuple
    leftIdentity: LeftIdentitySpec[LT, LM, LR]
    rightIdentity: RightIdentitySpec[RT, RR]
    associativity: AssociativitySpec[AA, AB, AMA, AMB, AR]



func leftIdentitySpec* [T; M; R](
  initial: T;
  f: T -> M;
  runArg: R
): LeftIdentitySpec[T, M, R] =
  (initial, f, runArg)


func rightIdentitySpec* [T; R](
  expected: T;
  runArg: R
): RightIdentitySpec[T, R] =
  (expected, runArg)


func associativitySpec* [A; B; MA; MB; R](
  initial: A;
  f: A -> MA;
  g: B -> MB;
  runArg: R
): AssociativitySpec[A, B, MA, MB, R] =
  (initial, f, g, runArg)


func monadLawsSpec* [LT; LM; LR; RT; RR; AA; AB; AMA; AMB; AR](
  leftIdentity: LeftIdentitySpec[LT, LM, LR];
  rightIdentity: RightIdentitySpec[RT, RR];
  associativity: AssociativitySpec[AA, AB, AMA, AMB, AR]
): MonadLawsSpec[LT, LM, LR, RT, RR, AA, AB, AMA, AMB, AR] =
  (leftIdentity, rightIdentity, associativity)



template checkLeftIdentity* [T; M; R](spec: LeftIdentitySpec[T, M, R]): bool =
  spec
    .initial
    .lift()
    .flatMap(spec.f)
    .run(spec.runArg)
    .`==`(spec.f(spec.initial).run(spec.runArg))



template checkRightIdentity* [T, R](spec: RightIdentitySpec[T, R]): bool =
  spec
    .expected
    .lift()
    .flatMap(lift[T])
    .run(spec.runArg)
    .`==`(spec.expected.lift().run(spec.runArg))


template checkAssociativity* [A; B; MA; MB; R](
  spec: AssociativitySpec[A, B, MA, MB, R]
): bool =
  spec
    .initial
    .lift()
    .flatMap(spec.f)
    .flatMap(spec.g)
    .run(spec.runArg)
    .`==`(
      spec
        .initial
        .lift()
        .flatMap(spec.f.chain(m => m.flatMap(spec.g)))
        .run(spec.runArg)
    )


template checkMonadLaws* [LT; LM; LR; RT; RR; AA; AB; AMA; AMB; AR](
  spec: MonadLawsSpec[LT, LM, LR, RT, RR, AA, AB, AMA, AMB, AR]
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
