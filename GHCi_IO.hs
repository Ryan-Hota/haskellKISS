module GHCi_IO (
    loadInShell ,
    loadInGHCi
) where

import Options (options)
import Directory_IO (fileTreeAlong, getCurrentDirectory)
import Target_IO (Target, targetPath)
import FilePath (unWrap, makeAbsolute)
import System.Process (shell ,createProcess, waitForProcess)
import Utilities ((|>))
import Shadow_IO (withShadowOf)
import Control.Monad (void)
import OS_IO ( clearScreenCommand )
import System.IO (readFile')
import System.Environment (setEnv, getEnv)
import Data.List (partition)

ghciOptions :: [String] -> ([String],[String])
ghciOptions = 
    filter (take 1|>(/="-"))
    |> partition (take 10|>(==":set -pgmL"))

getUserCmds :: Target -> IO String
getUserCmds target = do
    dir <- unWrap <$> getCurrentDirectory
    fileTreeAlongTarget <- fileTreeAlong $ targetPath target
    let opts = ghciOptions ( options fileTreeAlongTarget )
    pure $ unlines (
        [ ":! " ++ clearScreenCommand ]
        ++ [ "::set -i" ++ dir ]
        ++ fst opts
        ++ [ "::load " ++ show ( unWrap $ targetPath target ) ]
        ++ [""]
        ++ snd opts
        )

loadInShell :: Target -> IO ()
loadInShell target = void.waitForProcess.(\(_,_,_,x)->x) =<< do

    withShadowOf target $ do
        userCmds <- getUserCmds target
        writeFile ".ghci" (
            "import qualified System.Environment as SystemEnvironmentForHaskellKISS\n"
            ++ "import qualified Control.Applicative as ControlApplicativeForHaskellKISS\n"
            ++ "::def! load (\\path->let pure = ControlApplicativeForHaskellKISS.pure ; (<|>) = (ControlApplicativeForHaskellKISS.<|>) in pure ( \":! haskellKISS  \" <|> path <|> \" loadGHCiInGHCi\" <|> \"\\n:reload\" ) )\n"
            ++ "::def! l (\\path->let pure = ControlApplicativeForHaskellKISS.pure ; (<|>) = (ControlApplicativeForHaskellKISS.<|>) in pure ( \":load \" <|> path ) )\n"
            ++ "::def! reload (\\_->do{script<-SystemEnvironmentForHaskellKISS.getEnv \"HASKELLKISS_GHCI_SCRIPT\" ; ControlApplicativeForHaskellKISS.pure (\"::script \" ControlApplicativeForHaskellKISS.<|> script)})\n"
            ++ "::def! r (\\_-> ControlApplicativeForHaskellKISS.pure \":reload\")\n"
            ++ userCmds
            )
        setEnv "HASKELLKISS_GHCI_SCRIPT" . unWrap =<< makeAbsolute ".ghci"

    scriptFilePath <- getEnv "HASKELLKISS_GHCI_SCRIPT"
    (createProcess.shell) ( "ghci -ghci-script " ++ show scriptFilePath )

loadInGHCi :: Target -> IO ()
loadInGHCi target = void $ do

    userCmdsBefore <- readFile' =<< getEnv "HASKELLKISS_GHCI_SCRIPT"

    withShadowOf target $ do
        userCmds <- getUserCmds target
        writeFile ".ghci" $ if dropWhile (not.null) ( lines userCmdsBefore ) == dropWhile (not.null) ( lines userCmds )
            then userCmds
            else ":! haskellKISS " ++ show ( unWrap $ targetPath target ) ++ " loadGHCiInShell" ++ "\n::quit\n"
        setEnv "HASKELLKISS_GHCI_SCRIPT" . unWrap =<< makeAbsolute ".ghci"

    -- >>> :reload
