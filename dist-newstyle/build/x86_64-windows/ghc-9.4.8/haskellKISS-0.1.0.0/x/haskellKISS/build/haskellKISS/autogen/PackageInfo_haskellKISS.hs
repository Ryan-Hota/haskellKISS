{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskellKISS (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskellKISS"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "an over-engineered hls config"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
