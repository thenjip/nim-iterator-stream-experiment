##[
  Functions to alias the explicit conversion operator.
]##



func convert* [A](a: A; B: typedesc): B =
  a.B


func convert* [A; B](a: A): B =
  a.convert(B)


func `as`* [A](a: A; B: typedesc): B =
  a.convert(B)


func `as`* [A; B](a: A): B =
  a.convert(B)



when isMainModule:
  import std/[os, unittest]



  proc main () =
    suite currentSourcePath().splitFile().name:
      discard



  main()
