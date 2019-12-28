import nim_iterator_stream_experiment/[identity, optional, stream, unit, utils]



export identity, optional, stream, unit, utils



when isMainModule:
  import std/[os, strformat, sequtils, strutils]



  func moduleDir (): string =
    let (dir, file, _) = currentSourcePath().splitFile()

    dir / file


  proc exec (cmdLine: string) =
    echo(cmdLine)

    if cmdLine.execShellCmd() != QuitSuccess:
      osLastError().raiseOSError("Command failed:\n" & cmdLine)



  let cmdLine = @[getCurrentCompilerExe(), $'c', "-r",]

  for kind, path in moduleDir().walkDir(false):
    if kind == pcFile and path.endsWith(fmt"{ExtSep}nim"):
      cmdLine.`&`(path).foldl(a & " " & b.quoteShell(), "").exec()
