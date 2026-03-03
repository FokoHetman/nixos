{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_basement (
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
version = Version [0,0,16] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/basement-0.0.16-710033b928b437f26effd2d96cdfc763df6e9fe6e91ecc330b981ec9b3dfab1a/etc"

getBinDir     = catchIO (getEnv "basement_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "basement_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "basement_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "basement_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "basement_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "basement_sysconfdir") (\_ -> return sysconfdir)



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
