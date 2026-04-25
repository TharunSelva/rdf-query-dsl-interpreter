{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_rdf_query_dsl_interpreter (
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
bindir     = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\bin"
libdir     = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\lib\\x86_64-windows-ghc-9.4.8\\rdf-query-dsl-interpreter-0.1.0.0-qeIqgEpb58333F9oBDZnw-rdf-query-dsl-interpreter"
dynlibdir  = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\share\\x86_64-windows-ghc-9.4.8\\rdf-query-dsl-interpreter-0.1.0.0"
libexecdir = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\libexec\\x86_64-windows-ghc-9.4.8\\rdf-query-dsl-interpreter-0.1.0.0"
sysconfdir = "C:\\Users\\tharu\\OneDrive\\Documents\\Education\\CS Soton\\Year 2\\Semester 2\\PLC\\Coursework\\plc_cw_repo\\.stack-work\\install\\a436c4b1\\etc"

getBinDir     = catchIO (getEnv "rdf_query_dsl_interpreter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "rdf_query_dsl_interpreter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "rdf_query_dsl_interpreter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "rdf_query_dsl_interpreter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rdf_query_dsl_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rdf_query_dsl_interpreter_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
