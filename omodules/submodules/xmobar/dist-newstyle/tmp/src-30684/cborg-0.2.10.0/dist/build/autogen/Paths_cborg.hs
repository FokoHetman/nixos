{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cborg (
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
version = Version [0,2,10,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/cborg-0.2.10.0-64567eb9fe77a9735cfad888aaa449f1b5cc713e5cf962ff41726ffe3989adc9/etc"

getBinDir     = catchIO (getEnv "cborg_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cborg_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cborg_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cborg_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cborg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cborg_sysconfdir") (\_ -> return sysconfdir)



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
