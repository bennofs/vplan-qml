{-# LANGUAGE OverloadedStrings #-}
module Qt where

import           Control.Lens hiding (Action)
import           Data.Foldable (foldMap)
import           Data.List.Split.Lens
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Development.Shake
import           Development.Shake.FilePath
import           Shake.Configure

qt5module :: String -> Finder
qt5module modName = defPackage ("Qt5" <> modName) $ foldMap (fmap handleFlags . pkgConfigFind) pkgConfigNames
  where pkgName = "Qt5" <> modName
        pkgConfigNames = [pkgName, "Qt" <> modName]
        handleFlags conf
          | elemOf (splittingOn " " $ variables . ix "qt_config" . traverse) "reduce_relocations" conf = conf & cflags %~ ("-fPIC":)
          | otherwise = conf

qt5core, qt5quick, qt5gui, qt5qml, qt5widgets :: Finder
qt5core = qt5module "Core"
qt5quick = qt5module "Quick"
qt5gui = qt5module "Gui"
qt5widgets = qt5module "Widgets"
qt5qml = qt5module "Qml"

qt5moc :: FilePath -> [String] -> PackageConfig -> Action ()
qt5moc file options conf = system' moc $ file : options ++ opts
  where moc = fromMaybe (error "Qt5 Moc not found") $ conf ^? variables . ix "host_bins" . to (</> "moc")
        opts = mconcat
          [ concatMap (flag "-I") $ conf ^. includePaths
          , concatMap define $  M.toList $ conf ^. defines
          ] 
        flag x v = [x,v]
        define (k,v) = flag "-D" $ k <> "=" <> v

qt5rcc :: FilePath -> FilePath -> PackageConfig -> Action ()
qt5rcc file out conf = system' rcc $ [file, "-o", out]
  where rcc = fromMaybe (error "Qt5 resource compiler not found") $ conf ^? variables . ix "host_bins" . to (</> "rcc")
  
  
