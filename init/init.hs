{-# OPTIONS_GHC -Wall #-}

import System.FilePath( (</>), takeDrive, (<.>), takeDirectory, isDrive )
import System.Directory (copyFile, getHomeDirectory, getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile, exeExtension)
import System.Process (CreateProcess(..), createProcess, shell, StdStream (CreatePipe), waitForProcess)
import System.IO(hGetContents', hGetContents)
import Control.Monad (filterM, void)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Exit (ExitCode (..))
import Prelude hiding (error)
import qualified Prelude

error :: String -> any
error = Prelude.error . ("\ESC[91m"++) . (++"\ESC[0m")

warning :: String -> IO Bool
warning = (True <$) . putStrLn . ("\ESC[33m"++) . (++"\ESC[0m")

run :: String -> IO Bool
run str = do
    (_,_,Just herr,ph) <- createProcess ((shell str){std_err=CreatePipe})
    putStrLn =<< hGetContents herr
    exitCode <- waitForProcess ph
    pure $ case exitCode of
        ExitSuccess -> True
        ExitFailure _ -> False

installVscodeExtension :: String -> IO Bool
installVscodeExtension extId = do

    vscodeAccessible <- run "code --help"
    if vscodeAccessible
        then run ("code --install-extension "++extId)
        else vscodeWarning


compileHaskellKISSAt :: FilePath -> IO ()
compileHaskellKISSAt exeDir = do

    currentDir <- getCurrentDirectory

    maybeCorrectDir <- fmap listToMaybe . filterM (doesFileExist.(</>"hie.yaml")) . takeWhile (not.isDrive) . iterate takeDirectory $ currentDir
    let correctDir = fromMaybe (error ("Could not find project root (a folder containing \"hie.yaml\") among the ancestor folders of the current working folder "++show currentDir)) maybeCorrectDir

    let localExeDir = correctDir</>"executable"
    listDirectory localExeDir >>= mapM_ (copyFile<$>(localExeDir</>)<*>(exeDir</>))

    compilationSucceeded <- run ( "ghc -i"++exeDir++" -O2 "++show (exeDir</>"haskellKISS.hs") )
    if compilationSucceeded
        then mapM_ (removeFile.(exeDir</>)) . filter (/=("haskellKISS"<.>exeExtension)) =<< listDirectory exeDir
        else error "ghc compilation of haskellKISS failed"

data To = To

copy :: FilePath -> To -> FilePath -> IO ()
copy cabalExecutable To exeDir = do

    cabalAccessible <-  run "cabal --help"
    if cabalAccessible then pure () else error "cabal cannot be accessed here"

    _ <- run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDir = drop 12 $ last $ lines out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    if exeExists
        then copyFile (cabalInstallDir</>cabalExecutable<.>exeExtension) (exeDir</>cabalExecutable<.>exeExtension)
        else error ("Search for "++show (cabalExecutable<.>exeExtension)++" in "++show cabalInstallDir++" failed.")

main :: IO ()
main = do

    primaryDrive <- takeDrive <$> getHomeDirectory

    let exeDir = primaryDrive</>"haskellKISSExecutables"
    createDirectoryIfMissing True exeDir
    -- writeFile ".."</>"hie,yaml"

    -- _ <- if os=="mingw32" 
        -- then run "powershell -ExecutionPolicy Bypass -File addToPath.ps1"
        -- else run "echo \"remember to add "++show (show exeDir)++" to PATH\""

    vscodeAccessible <- run "code --help"
    if vscodeAccessible
        then do 
            _ <- installVscodeExtension "mogeko.haskell-extension-pack"
            _ <- installVscodeExtension "formulahendry.code-runner"
            listDirectory (".."</>".vscode") >>= mapM_ (copyFile<$>((".."</>".vscode")</>)<*>(".vsocde"</>))
        else void $ warning "Could not access VS Code, therfore skipping VS Code stuff"
            

    alreadyCompiled <- doesFileExist (exeDir</>"haskellKISS"<.>exeExtension)
    if alreadyCompiled
        then pure ()
        else compileHaskellKISSAt exeDir

    hlintAlreadyMoved <- doesFileExist (exeDir</>"hlint"<.>exeExtension)
    if hlintAlreadyMoved
        then pure ()
        else copy "hlint" To exeDir

    unlitAlreadyMoved <- doesFileExist (exeDir</>"markdown-unlit"<.>exeExtension)
    if unlitAlreadyMoved
        then pure ()
        else copy "markdown-unlit" To exeDir

    putStrLn "\ESC[32mhaskellKISS installation complete!\ESC[0m"

    _ <- if vscodeAccessible
        then do
            firstExists <- doesFileExist "first.hs"
            if firstExists
                then run ("code \"."</>"\" -n -g first.hs:0:0")
                else warning "did not find file \"first.hs\", so not opening VS Code"
        else vscodeWarning

    pure ()