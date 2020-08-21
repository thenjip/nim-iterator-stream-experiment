when isMainModule:
  import std/[os, strformat, strutils]



  proc main () =
    withDir projectDir():
      for args in [
        @["a", "b", "c"],
        @["--abc:on", "a.nim"],
        @["-a", "'a bc'", "abc", "--run"]
      ]:
        @[$'e', fmt"args{ExtSep}nims"].`&`(args).join($' ').selfExec()



  main()
