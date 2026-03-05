{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_happy_lib (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "happy_lib"
version :: Version
version = Version [2,1,7] []

synopsis :: String
synopsis = "Happy is a parser generator for Haskell implemented using this library"
copyright :: String
copyright = "(c) Andy Gill, Simon Marlow"
homepage :: String
homepage = "https://www.haskell.org/happy/"
