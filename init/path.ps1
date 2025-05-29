$Path = [Environment]::GetEnvironmentVariable("PATH", "Machine") + [IO.Path]::PathSeparator + "C:\\haskellKISSExecutables"
[Environment]::SetEnvironmentVariable( "Path", $Path, "Machine" )