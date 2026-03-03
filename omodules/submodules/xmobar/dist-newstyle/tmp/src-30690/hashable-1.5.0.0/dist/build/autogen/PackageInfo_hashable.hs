{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hashable (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hashable"
version :: Version
version = Version [1,5,0,0] []

synopsis :: String
synopsis = "A class for types that can be converted to a hash value"
copyright :: String
copyright = ""
homepage :: String
homepage = "http://github.com/haskell-unordered-containers/hashable"
