when isMainModule:
  import std/[os, strformat, sequtils, strutils]



  func moduleDir (): string =
    const srcPath = currentSourcePath().splitFile()

    srcPath.dir / srcPath.name


  proc exec (cmdLine: string) =
    echo(cmdLine)

    if cmdLine.execShellCmd() != QuitSuccess:
      osLastError().raiseOSError("Command failed:\n" & cmdLine)



  let cmdLine = @[getCurrentCompilerExe(), $'c', "-r",]

  for path in moduleDir().walkDirRec():
    if path.endsWith(fmt"{ExtSep}nim"):
      cmdLine.`&`(path).foldl(a & " " & b.quoteShell(), "").exec()
