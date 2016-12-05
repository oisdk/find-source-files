module Test.Doctest.Find (findSourceFiles) where

import           Distribution.ModuleName               hiding (main)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity

import           System.Directory
import           System.FilePath

import           Control.Monad.List

findSourceFiles :: IO [FilePath]
findSourceFiles =
    runListT $
    do curDir <- lift getCurrentDirectory
       let getCabalFile =
               map (curDir </>) . filter ((".cabal" ==) . takeExtension)
       cabalFile <- (ListT . fmap getCabalFile . getDirectoryContents) curDir
       packDesc <- lift $ readPackageDescription normal cabalFile
       let getLocs em lbi =
               ListT $
               filterM
                   doesFileExist
                   [ curDir </> dir </> mdn <.> ext
                   | dir <- hsSourceDirs lbi
                   , mdn <- map toFilePath em
                   , ext <- ["lhs", "hs"] ]
       condLib <- maybe (ListT (pure [])) pure (condLibrary packDesc)
       let libInfo = condTreeData condLib
       getLocs (exposedModules libInfo) (libBuildInfo libInfo)
