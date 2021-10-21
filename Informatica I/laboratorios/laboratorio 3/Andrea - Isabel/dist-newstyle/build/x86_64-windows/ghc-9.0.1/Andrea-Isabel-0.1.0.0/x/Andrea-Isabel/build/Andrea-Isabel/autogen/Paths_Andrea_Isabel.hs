{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Andrea_Isabel (
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

bindir     = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Andrea-Isabel-0.1.0.0-inplace-Andrea-Isabel"
dynlibdir  = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Andrea-Isabel-0.1.0.0"
libexecdir = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\Andrea-Isabel-0.1.0.0-inplace-Andrea-Isabel\\x86_64-windows-ghc-9.0.1\\Andrea-Isabel-0.1.0.0"
sysconfdir = "C:\\Users\\Andreita\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Andrea_Isabel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Andrea_Isabel_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Andrea_Isabel_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Andrea_Isabel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Andrea_Isabel_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Andrea_Isabel_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
