{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_project (
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
bindir     = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/bin"
libdir     = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/lib/x86_64-linux-ghc-9.2.5/project-0.1.0.0-JTV4RSNsEpg39oMu0AAUZ0-project"
dynlibdir  = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/share/x86_64-linux-ghc-9.2.5/project-0.1.0.0"
libexecdir = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/libexec/x86_64-linux-ghc-9.2.5/project-0.1.0.0"
sysconfdir = "/home/max/Documents/fun/hs/project/.stack-work/install/x86_64-linux/2f14c6a885baaaa007b9905d74eb446dafa6ee42d540cd86b938979e41fd8efa/9.2.5/etc"

getBinDir     = catchIO (getEnv "project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_sysconfdir") (\_ -> return sysconfdir)




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