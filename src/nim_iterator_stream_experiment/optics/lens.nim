import plens



export plens



type
  Lens* [S; T] = PLens[S, T, T, S]



when isMainModule:
  import private/[lens_test_common]
  import ../monad/[io, reader]

  import std/[os, strutils, sugar, unittest]



  suite currentSourcePath().splitFile().name:
    test """Reading a "Street"'s name through a lens should return its actual name.""":
      proc doTest (nameLens: Lens[Street, string]; street: Street) =
        let
          expected = street.readName()
          sut = nameLens.read().run(street)

        check:
          sut == expected


      doTest(name(Street), street("aaBc01a", 1))


    test """Modifying a "Street"'s name through a lens should return a "Street" with the modified name and same number.""":
      func modifyName (name: string): string =
        name.toUpperAscii()


      proc doTest (
        nameLens: Lens[Street, string];
        street: Street;
        modifier: string -> string
      ) =
        let
          expected = street(street.readName().modifyName(), street.readNumber())
          sut = nameLens.modify(modifier).run(street)

        check:
          sut == expected


      doTest(name(Street), street("aaBc01a", 15), modifyName)



    test """Writing a "Street"'s name through a lens should return a "Street" with the new name and same number.""":
      proc doTest (
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


      doTest(name(Street), street("abc", 0), "bca")
