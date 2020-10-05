import std/[sugar]



type
  Pair* [A; B] =
    tuple[first: A; second: B]
    ## Since 0.4.0.



proc apply* [A1; A2; B1; B2](
  self: Pair[A1, A2];
  f1: A1 -> B1;
  f2: A2 -> B2
): Pair[B1, B2] =
  ## Since 0.4.0.
  (f1(self.first), f2(self.second))


proc reduce* [A; B; R](self: Pair[A, B]; reducer: (A, B) -> R): R =
  ## Since 0.4.0.
  reducer(self.first, self.second)



when isMainModule:
  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      discard



  main()
