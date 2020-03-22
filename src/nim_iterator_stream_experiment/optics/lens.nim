import plens



export plens



type
  Lens* [S; T] = PLens[S, T, T, S]



when isMainModule:
  import lens_test_common
  import ../monad/[io, reader]

  import std/[os, strutils, unittest]



  suite currentSourcePath().splitFile().name:
    test "read: 'Street' name":
      proc readTest (nameLens: Lens[Street, string]; street: Street) =
        let
          expected = street.readName()
          sut = nameLens.read().run(street)

        check:
          sut == expected


      readTest(Street.focusOnName(), street("aaBc01a", 1))


    test "modify: 'Street' name":
      func modifyName (name: string): string =
        name.toUpperAscii()


      proc modifyTest (
        nameLens: Lens[Street, string];
        street: Street;
        modifier: Reader[string, string]
      ) =
        let
          expected = street(street.readName().modifyName(), street.readNumber())
          sut = nameLens.modify(modifier).run(street)

        check:
          sut == expected


      modifyTest(Street.focusOnName(), street("aaBc01a", 15), modifyName)



    test "write: 'Street' name":
      proc writeTest (
        nameLens: Lens[Street, string];
        street: Street;
        newName: string
      ) =
        require:
          street.readName() != newName

        let
          expected = street(newName, street.readNumber())
          sut = nameLens.write(newName.toIO()).run(street)

        check:
          sut == expected


      writeTest(Street.focusOnName(), street("abc", 0), "bca")
