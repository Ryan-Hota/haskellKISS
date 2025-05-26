module GHCi_IO (
    loadInShell ,
    loadInGHCi
) where

import Options (options)
import Modules (modules)
import Directory_IO (fileTreeAlong, mkLinkAt, getCurrentDirectory)
import Target_IO (Target, targetPath)
import FilePath (unWrap, (</>))
import Directory (path)
import System.Process (shell ,createProcess, waitForProcess)
import Utilities ((|>))
import Shadow_IO (withShadowOf)
import Control.Monad (void, forM_)
import OS_IO ( clearScreenCommand )
import System.IO (readFile')
import System.Environment (setEnv, getEnv)

ghciOptions :: [String] -> [String]
ghciOptions = filter (take 1|>(/="-"))

main :: Target -> IO ()
main target = 
    do
        dir <- getCurrentDirectory
        fileTreeAlongTarget <- fileTreeAlong $ targetPath target
        forM_ ( modules fileTreeAlongTarget ) ( path |> mkLinkAt dir )
        writeFile ".ghci" $ unlines (
            ( ":! " ++ clearScreenCommand )
            : ( ":set -i" ++ unWrap dir )
            : ( ":load " ++ show ( unWrap $ targetPath target ) )
            : ghciOptions ( options fileTreeAlongTarget )
            )
        return ()

loadInShell :: Target -> IO ()
loadInShell target =
    ( void . waitForProcess . (\(_,_,_,x)->x) ) =<< ( createProcess . shell ) =<< do
        ( shadowDir , _ ) <- withShadowOf target $ main target
        setEnv "HASKELLKISS_GHCI_SCRIPT" ( unWrap ( shadowDir </> ".ghci" ) )
        return (
            "ghci -ghci-script " ++ show ( unWrap ( shadowDir </> ".ghci" ) )
            )

loadInGHCi :: Target -> IO ()
loadInGHCi target = let mainIO = main target in
    putStrLn =<< do
        ( shadowDir , scriptUnchanged ) <- withShadowOf target $ do
            scriptBefore <- readFile' =<< getEnv "HASKELLKISS_GHCI_SCRIPT"
            mainIO
            scriptAfter <- readFile' ".ghci"
            return ( drop 3 ( lines scriptBefore ) == drop 3 ( lines scriptAfter ) )
        return (
            if scriptUnchanged
                then ":script " ++ show ( unWrap ( shadowDir </> ".ghci" ) )
                else ":! ghci -ghci-script " ++ show ( unWrap $ targetPath target ) ++ "\n:quit"
            )
