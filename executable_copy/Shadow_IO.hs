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
import Directory_IO ( withCurrentDirectory, removePathForcibly,fileTreeAlong, mkLinkAt)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDrive, isPathSeparator)
import Control.Monad ((>=>), forM_, void)
import Modules(modules)
import Directory(path)

root :: Target -> IO AbsoluteFilePath
root target = makeAbsolute ( takeDrive $ unWrap $ targetPath target ) <&> (</>"haskellKISSShadow")

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

withShadowOf :: Target -> IO a -> IO ()
withShadowOf target action = void $ do
    shadowDir <- shadowOf target
    createDirectoryIfMissing True $ unWrap shadowDir
    withCurrentDirectory shadowDir ( do
        fileTreeAlongTarget <- fileTreeAlong $ targetPath target
        forM_ ( modules fileTreeAlongTarget ) ( path |> mkLinkAt shadowDir )
        action
        )

clean :: Target -> IO ()
clean = root >=> removePathForcibly