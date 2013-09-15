{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Haskell where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Development.Shake.FilePath
import           Shake.Configure
import           Shelly hiding ((</>))

haskell :: Finder
haskell = defPackage "haskell" $ shellyNoDir $ silently $ do
  libdir <- T.unpack . T.strip <$> cmd "ghc" "--print-libdir"
  return $ mempty
    & includePaths .~ [libdir </> "include"]
    & variables . at "ghc" ?~ "ghc"
           
parseImports :: [String] -> [String]
parseImports = mapMaybe (fmap T.unpack . preview _Right . parseOnly imp . T.pack)
  where imp = "import" .*> many1 space *> optional ("qualified" .*> many1 space) *> takeWhile1 (not . isSpace)
