{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_CovidDataAnalyse (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/runner/.cabal/bin"
libdir     = "/home/runner/.cabal/lib/x86_64-linux-ghc-9.0.2/CovidDataAnalyse-0.1.0.0-inplace-CovidDataAnalyse"
dynlibdir  = "/home/runner/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/runner/.cabal/share/x86_64-linux-ghc-9.0.2/CovidDataAnalyse-0.1.0.0"
libexecdir = "/home/runner/.cabal/libexec/x86_64-linux-ghc-9.0.2/CovidDataAnalyse-0.1.0.0"
sysconfdir = "/home/runner/.cabal/etc"

getBinDir     = catchIO (getEnv "CovidDataAnalyse_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "CovidDataAnalyse_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "CovidDataAnalyse_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "CovidDataAnalyse_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CovidDataAnalyse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CovidDataAnalyse_sysconfdir") (\_ -> return sysconfdir)




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
