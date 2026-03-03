{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_regex_base (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "regex_base"
version :: Version
version = Version [0,94,0,3] []

synopsis :: String
synopsis = "Common \"Text.Regex.*\" API for Regex matching"
copyright :: String
copyright = "Copyright (c) 2006, Christopher Kuklewicz"
homepage :: String
homepage = "https://wiki.haskell.org/Regular_expressions"
