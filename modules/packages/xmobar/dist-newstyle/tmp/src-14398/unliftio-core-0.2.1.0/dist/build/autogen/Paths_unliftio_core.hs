{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_unliftio_core (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/unliftio-core-0.2.1.0-07cd4601b4b763b6660cafcc0bb1ecaff8be7bf35c0436a09e9c1a229f26fcb2/etc"

getBinDir     = catchIO (getEnv "unliftio_core_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "unliftio_core_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "unliftio_core_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "unliftio_core_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unliftio_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unliftio_core_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
