module GHCi_IO (
    loadInShell ,
    loadInGHCi
) where

import Options (options)
import Directory_IO (fileTreeAlong, getCurrentDirectory)
import Target_IO (Target, targetPath)
import FilePath (unWrap, makeAbsolute)
import System.Process (shell ,createProcess)
import Utilities ((|>))
import Shadow_IO (withShadowOf)
import Control.Monad (void)
import OS_IO ( clearScreenCommand )
import System.IO (readFile')
import System.Environment (setEnv, getEnv)

ghciOptions :: [String] -> [String]
ghciOptions = filter (take 1|>(/="-"))

getUserCmds :: Target -> IO String
getUserCmds target = do
    dir <- unWrap <$> getCurrentDirectory
    fileTreeAlongTarget <- fileTreeAlong $ targetPath target
    return $ unlines (
        ( "::! " ++ clearScreenCommand )
        : ( "::set -i" ++ dir )
        : ( "::load " ++ show ( unWrap $ targetPath target ) )
        : ( "" )
        : ghciOptions ( options fileTreeAlongTarget )
        )

loadInShell :: Target -> IO ()
loadInShell target = void $ do

    withShadowOf target $ do
        userCmds <- getUserCmds target
        writeFile ".ghci" (
            "import qualified System.Environment as SystemEnvironmentForHaskellKISS\n"
            ++ "import qualified Control.Applicative as ControlApplicativeForHaskellKISS\n"
            ++ "::def! load (\\path->\"::! haskellKISS \" ControlApplicativeForHaskellKISS.<|> path ControlApplicativeForHaskellKISS.<|> \" loadGHCiInGHCi\")"
            ++ "::def! l (\\_-> ControlApplicativeForHaskellKISS.pure \":load\")"
            ++ "::def! reload (\\_->do{script<-SystemEnvironmentForHaskellKISS.getEnv \"HASKELLKISS_GHCI_SCRIPT\" ; ControlApplicativeForHaskellKISS.pure (\"::script \" ControlApplicativeForHaskellKISS.<|> script)})\n"
            ++ "::def! r (\\_-> ControlApplicativeForHaskellKISS.pure \":reload\")"
            ++ userCmds
            )
        setEnv "HASKELLKISS_GHCI_SCRIPT" =<< ( unWrap <$> makeAbsolute ".ghci" )

    scriptFilePath <- getEnv "HASKELLKISS_GHCI_SCRIPT"
    (createProcess.shell) ( "ghci -ghci-script " ++ show scriptFilePath )

loadInGHCi :: Target -> IO ()
loadInGHCi target = void $ do

    userCmdsBefore <- readFile' =<< getEnv "HASKELLKISS_GHCI_SCRIPT"

    withShadowOf target $ do
        userCmds <- getUserCmds target
        writeFile ".ghci" $ if ( dropWhile (not.null) ( lines userCmdsBefore ) == dropWhile (not.null) ( lines userCmds ) )
            then userCmds
            else "::! haskellKISS " ++ show ( unWrap $ targetPath target ) ++ " loadGHCiInShell" ++ "\n::quit"
        setEnv "HASKELLKISS_GHCI_SCRIPT" =<< ( unWrap <$> makeAbsolute ".ghci" )

    -- >>> :reload
