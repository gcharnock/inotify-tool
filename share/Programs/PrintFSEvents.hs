
module Programs.PrintFSEvents where

import Effects.Filesystem
import Effects.FileWatcher
import Effects.Notice

import Effects.FileSystem.Posix
import Effects.FileWatcher.INotify
import Effects.Notice.Console

main :: IO ()
main = runM . runConsoleNotice . runFSPosix . runINotify $ program

