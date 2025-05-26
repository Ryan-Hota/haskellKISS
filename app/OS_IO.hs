{-# LANGUAGE CPP #-}

module OS_IO (
    mkHardLink
, clearScreenCommand) where

import System.Directory(createFileLink)
import System.Info(os)

#if defined(mingw32_HOST_OS)
import System.Win32.HardLink (createHardLink)
-- | source ->  destination -> action
mkHardLink :: FilePath -> FilePath -> IO ()
mkHardLink = createHardLink
#else
import System.Posix.Files
-- | destination -> source -> action
mkHardLink :: FilePath -> FilePath -> IO ()
mkHardLink = createFileLink
#endif

clearScreenCommand :: String
clearScreenCommand = 
    if os=="mingw32" 
        then "cls"
        else "clear"