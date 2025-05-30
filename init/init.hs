{-# OPTIONS_GHC -Wall #-}

import System.FilePath( (</>), takeDrive, (<.>), takeDirectory, isDrive )
import System.Directory (copyFile, getHomeDirectory, getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile, exeExtension, removeDirectory, doesDirectoryExist, setCurrentDirectory)
import System.Process (CreateProcess(..), createProcess, shell, StdStream (CreatePipe), waitForProcess)
import System.IO(hGetContents', hGetContents)
import Control.Monad (filterM, when, void, unless)
import Data.Maybe (listToMaybe, fromMaybe)
import System.Exit (ExitCode (..))
import Prelude hiding (error)
import qualified Prelude
import Data.Functor ((<&>))

error :: String -> any
error = Prelude.error . ("\ESC[91m"++) . (++"\ESC[0m")

-- warning :: String -> IO Bool
-- warning = (True <$) . putStrLn . ("\ESC[33m"++) . (++"\ESC[0m")

run :: String -> IO Bool
run str = do
    (_,_,Just herr,ph) <- createProcess ((shell str){std_err=CreatePipe})
    putStrLn =<< hGetContents herr
    exitCode <- waitForProcess ph
    pure $ case exitCode of
        ExitSuccess -> True
        ExitFailure _ -> False

installVscodeExtension :: String -> IO Bool
installVscodeExtension extId = run ("code --install-extension "++extId)

compileHaskellKISSAt :: FilePath -> IO ()
compileHaskellKISSAt exeDir = do

    root <- takeDirectory <$> getCurrentDirectory

    let localExeDir = root</>"executable"
    listDirectory localExeDir >>= mapM_ (copyFile<$>(localExeDir</>)<*>(exeDir</>))

    compilationSucceeded <- run ( "ghc -i"++exeDir++" -O2 "++show (exeDir</>"haskellKISS.hs") )
    if compilationSucceeded
        then mapM_ (removeFile.(exeDir</>)) . filter (/=("haskellKISS"<.>exeExtension)) =<< listDirectory exeDir
        else error "ghc compilation of haskellKISS failed"

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++(exeDir</>"haskellKISS"{-<.>exeExtension-})++"\n    }\n  }")

data To = To

copy :: FilePath -> To -> FilePath -> IO ()
copy cabalExecutable To exeDir = do

    cabalAccessible <-  run "cabal --help"
    unless cabalAccessible $ error "cabal cannot be accessed here"

    void $ run ("cabal install "++cabalExecutable)

    (_,Just hout,_,_) <- createProcess ((shell "cabal path"){std_out=CreatePipe})
    out <- hGetContents' hout
    let cabalInstallDir = drop 12 $ last $ lines out

    exeExists <- doesFileExist (cabalInstallDir</>cabalExecutable<.>exeExtension)
    if exeExists
        then copyFile (cabalInstallDir</>cabalExecutable<.>exeExtension) (exeDir</>cabalExecutable<.>exeExtension)
        else error ("Search for "++show (cabalExecutable<.>exeExtension)++" in "++show cabalInstallDir++" failed.")

main :: IO ()
main = do

    maybeUserSpace <- fmap listToMaybe . filterM (doesFileExist.(</>"hie.yaml").takeDirectory) . takeWhile (not.isDrive) . iterate takeDirectory =<< getCurrentDirectory
    let userSpace = ( `fromMaybe` maybeUserSpace ) (error "Could not find user folder (sibling of \"hie.yaml\") among the ancestor folders of the current working folder" )
    setCurrentDirectory userSpace

    exeDir <- getHomeDirectory <&> (</>"haskellKISSExecutables") . takeDrive
    createDirectoryIfMissing True exeDir

    vscodeAccessible <- run "code --help"
    if vscodeAccessible
        then do
            void $ installVscodeExtension "mogeko.haskell-extension-pack"
            void $ installVscodeExtension "formulahendry.code-runner"
        else doesDirectoryExist ".vscode" >>= ( `when` removeDirectory ".vscode" )

    compileHaskellKISSAt exeDir

    writeFile (".."</>"hie"<.>"yaml") ("cradle: {\n  bios: {\n    program: "++(exeDir</>"haskellKISS"{-<.>exeExtension-})++"\n    }\n  }")

    -- _ <- if os=="mingw32" 
        -- then run "powershell -ExecutionPolicy Bypass -File addToPath.ps1"
        -- else run "echo \"remember to add "++show (show exeDir)++" to PATH\""
    
    hlintAlreadyMoved <- doesFileExist (exeDir</>"hlint"<.>exeExtension)
    unless hlintAlreadyMoved $ copy "hlint" To exeDir

    unlitAlreadyMoved <- doesFileExist (exeDir</>"markdown-unlit"<.>exeExtension)
    unless unlitAlreadyMoved  $ copy "markdown-unlit" To exeDir

    putStrLn "\ESC[32m\n\nhaskellKISS installation complete!\n\n\ESC[0m"

    firstExists <- doesFileExist "first.hs"
    let runWithoutWaiting = void.createProcess.shell in case ( vscodeAccessible , firstExists ) of
        (True,True) -> run ("code \"."</>"\"") >> runWithoutWaiting "code --goto first.hs:0:0"
        (False,True) -> runWithoutWaiting ("code \"."</>"\"")
        (True,False) -> runWithoutWaiting "ghci \"first.hs\""
        (False,False) -> runWithoutWaiting "ghci"