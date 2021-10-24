{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Tarea2 (
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

bindir     = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Tarea2-0.1.0.0-inplace-Tarea2"
dynlibdir  = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Tarea2-0.1.0.0"
libexecdir = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\Tarea2-0.1.0.0-inplace-Tarea2\\x86_64-windows-ghc-9.0.1\\Tarea2-0.1.0.0"
sysconfdir = "C:\\Users\\Pc Andres\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tarea2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tarea2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Tarea2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Tarea2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tarea2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tarea2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
