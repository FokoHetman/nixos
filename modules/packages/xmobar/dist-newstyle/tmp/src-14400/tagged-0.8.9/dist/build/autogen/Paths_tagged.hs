{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tagged (
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
version = Version [0,8,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/tagged-0.8.9-cf017cb0d26e04a428042440f81e4876248431d7f71ba338faeb4a24dfe54992/etc"

getBinDir     = catchIO (getEnv "tagged_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tagged_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tagged_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tagged_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tagged_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tagged_sysconfdir") (\_ -> return sysconfdir)



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
