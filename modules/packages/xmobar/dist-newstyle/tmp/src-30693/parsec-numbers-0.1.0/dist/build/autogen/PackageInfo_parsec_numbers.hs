{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_parsec_numbers (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "parsec_numbers"
version :: Version
version = Version [0,1,0] []

synopsis :: String
synopsis = "Utilities for parsing numbers from strings"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
