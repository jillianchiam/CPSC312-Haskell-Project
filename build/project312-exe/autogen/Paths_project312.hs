{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_project312 (
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

bindir     = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\bin"
libdir     = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\lib\\x86_64-windows-ghc-8.10.4\\project312-0.1.0.0-2zePk7Le1FA1YJJFAm8zBU-project312-exe"
dynlibdir  = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\share\\x86_64-windows-ghc-8.10.4\\project312-0.1.0.0"
libexecdir = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\libexec\\x86_64-windows-ghc-8.10.4\\project312-0.1.0.0"
sysconfdir = "C:\\Users\\jilli\\project312\\.stack-work\\install\\09ddad83\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project312_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project312_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "project312_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "project312_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project312_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project312_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
