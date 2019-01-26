{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ClientBindings (
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

bindir     = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/bin"
libdir     = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/lib/x86_64-osx-ghc-8.0.2/ClientBindings-0.1.0.0-F9PXp3gWYhKKsF5RUj3YMH"
dynlibdir  = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/share/x86_64-osx-ghc-8.0.2/ClientBindings-0.1.0.0"
libexecdir = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/libexec"
sysconfdir = "/Users/conorquinn1/Projects/ClientBindings/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ClientBindings_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ClientBindings_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ClientBindings_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ClientBindings_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ClientBindings_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ClientBindings_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
