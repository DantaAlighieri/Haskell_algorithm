{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_minihs (
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
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\minihs-1.0.0-inplace-minihs"
dynlibdir  = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\minihs-1.0.0"
libexecdir = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\minihs-1.0.0-inplace-minihs\\x86_64-windows-ghc-9.0.1\\minihs-1.0.0"
sysconfdir = "C:\\Users\\WangYao\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minihs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minihs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minihs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minihs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minihs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minihs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
