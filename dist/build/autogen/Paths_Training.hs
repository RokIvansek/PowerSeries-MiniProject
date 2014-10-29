module Paths_Training (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\Rok\\Documents\\Faks\\Funkcijsko_programiranje\\Haskell\\Formal\\Training\\.cabal-sandbox\\bin"
libdir     = "E:\\Rok\\Documents\\Faks\\Funkcijsko_programiranje\\Haskell\\Formal\\Training\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\Training-0.1.0.0"
datadir    = "E:\\Rok\\Documents\\Faks\\Funkcijsko_programiranje\\Haskell\\Formal\\Training\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\Training-0.1.0.0"
libexecdir = "E:\\Rok\\Documents\\Faks\\Funkcijsko_programiranje\\Haskell\\Formal\\Training\\.cabal-sandbox\\Training-0.1.0.0"
sysconfdir = "E:\\Rok\\Documents\\Faks\\Funkcijsko_programiranje\\Haskell\\Formal\\Training\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Training_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Training_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Training_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Training_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Training_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
