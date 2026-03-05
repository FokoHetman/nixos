{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_happy_lib (
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
version = Version [2,1,7] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/bin"
libdir     = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/lib"
dynlibdir  = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/lib"
datadir    = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/share"
libexecdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/libexec"
sysconfdir = "/home/foko/.local/state/cabal/store/ghc-9.8.4-4a67/happy-lib-2.1.7-l-backend-glr-b507330d010ec2cef4c2ca42575195361c50acecb4a9038a1646802a5ebaa982/etc"

getBinDir     = catchIO (getEnv "happy_lib_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "happy_lib_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "happy_lib_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "happy_lib_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "happy_lib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "happy_lib_sysconfdir") (\_ -> return sysconfdir)



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
