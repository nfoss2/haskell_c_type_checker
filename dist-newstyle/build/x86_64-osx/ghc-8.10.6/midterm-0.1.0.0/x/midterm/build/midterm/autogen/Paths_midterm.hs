{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_midterm (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nat/.cabal/bin"
libdir     = "/Users/nat/.cabal/lib/x86_64-osx-ghc-8.10.6/midterm-0.1.0.0-inplace-midterm"
dynlibdir  = "/Users/nat/.cabal/lib/x86_64-osx-ghc-8.10.6"
datadir    = "/Users/nat/.cabal/share/x86_64-osx-ghc-8.10.6/midterm-0.1.0.0"
libexecdir = "/Users/nat/.cabal/libexec/x86_64-osx-ghc-8.10.6/midterm-0.1.0.0"
sysconfdir = "/Users/nat/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "midterm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "midterm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "midterm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "midterm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "midterm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "midterm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
