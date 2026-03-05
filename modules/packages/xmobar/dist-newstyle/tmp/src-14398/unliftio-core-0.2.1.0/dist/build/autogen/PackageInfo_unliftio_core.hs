{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_unliftio_core (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "unliftio_core"
version :: Version
version = Version [0,2,1,0] []

synopsis :: String
synopsis = "The MonadUnliftIO typeclass for unlifting monads to IO"
copyright :: String
copyright = "2017-2020 FP Complete"
homepage :: String
homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme"
