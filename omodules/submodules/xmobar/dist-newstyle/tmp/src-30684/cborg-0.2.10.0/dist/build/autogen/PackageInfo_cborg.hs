{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cborg (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cborg"
version :: Version
version = Version [0,2,10,0] []

synopsis :: String
synopsis = "Concise Binary Object Representation (CBOR)"
copyright :: String
copyright = "2015-2019 Duncan Coutts,\n2015-2019 Well-Typed LLP,\n2015 IRIS Connect Ltd"
homepage :: String
homepage = ""
