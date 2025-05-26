{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Shadow_IO (
    withShadowOf,
    clean
) where

import FilePath (makeAbsolute, AbsoluteFilePath, unWrap, (</>))
import Target_IO (targetPath, Target)
import Utilities ((|>), (||>))
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Directory_IO ( withCurrentDirectory, removePathForcibly )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDrive, isPathSeparator)
import Control.Monad ((>=>))

root :: Target -> IO AbsoluteFilePath
root target = makeAbsolute ( takeDrive $ unWrap $ targetPath target ) <&> (</>"HaskellKISSMachinery") |> (</>"Shadow")

shadowOf :: Target -> IO AbsoluteFilePath
shadowOf target =
    target
    ||> targetPath
    |> unWrap
    |> concatMap replace
    |> pure
    |> ( (</>) <$> root target <*> )
    where
        replace               char
            | isPathSeparator char = "_SEP_"
            | isSpace         char = "_SPACE_"
            | ':' ==          char = "_COLON_"
            | '.' ==          char = "_DOT_"
            | otherwise            = [char]

withShadowOf :: Target -> IO r -> IO ( AbsoluteFilePath , r )
withShadowOf target action = do
    shadowDir <- shadowOf target
    createDirectoryIfMissing True $ unWrap shadowDir
    returnVal <- withCurrentDirectory shadowDir action
    return ( shadowDir , returnVal )

clean :: Target -> IO ()
clean = root >=> removePathForcibly