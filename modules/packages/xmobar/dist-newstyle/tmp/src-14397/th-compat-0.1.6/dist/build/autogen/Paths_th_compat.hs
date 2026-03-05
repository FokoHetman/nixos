{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_th_compat (
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
version = Version [0,1,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/th-compat-0.1.6-b0edade6cbed15aff4bf0b2d78bbc65b3de3cc5ecf0a752092cf70d19551ec8f/etc"

getBinDir     = catchIO (getEnv "th_compat_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "th_compat_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "th_compat_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "th_compat_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "th_compat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "th_compat_sysconfdir") (\_ -> return sysconfdir)



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
