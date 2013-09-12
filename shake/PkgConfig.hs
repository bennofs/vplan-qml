{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | pkg-config support functions
module PkgConfig where

import Shelly
import Data.Monoid
import Control.Lens
import Data.Attoparsec.Text
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as P
import qualified Data.Text as T

data PackageConfig = PackageConfig
  { _includePaths :: [P.FilePath]
  , _libraries    :: [T.Text]
  , _version      :: Maybe T.Text
  , _defines      :: [(T.Text, T.Text)]
  , _libraryPaths :: [P.FilePath]
  , _staticFlags  :: [T.Text]
  , _cflags       :: [T.Text]
  , _lflags       :: [T.Text]
  } deriving Show
makeLenses ''PackageConfig

reviewEmpty :: Monoid s => Lens s t a b -> b -> t
reviewEmpty l v = mempty & l .~ v

instance Monoid PackageConfig where
  mempty = PackageConfig mempty mempty mempty mempty mempty mempty mempty mempty
  mappend a b =
    a & includePaths <>~ b ^. includePaths
      & libraries <>~ b ^. libraries
      & defines <>~ b ^. defines
      & libraryPaths <>~ b ^. libraryPaths
      & staticFlags <>~ b ^. staticFlags
      & cflags <>~ b ^. cflags
      & lflags <>~ b ^. lflags
      & version . wrapping First <>~ b ^. version . wrapping First
  
-- | Use @pkg-config@ to get information about a package. Returns @Nothing@ if the package isn't found.
lookupPackage :: T.Text -> IO (Maybe PackageConfig)
lookupPackage package = shellyNoDir $ do
  let makeFlags = filter T.null . T.splitOn " "
  compilerFlags <- cmd "pkg-config" "--cflags" package
  linkerFlags <- cmd "pkg-config" "--libs" package
  v <- cmd "pkg-config" "--modversion" package
  static <- makeFlags <$> cmd "pkg-config" "--static" package  
  return $ fmap (set version (Just v) . set staticFlags static) $ parseCompilerFlags compilerFlags <> parseLinkerFlags linkerFlags

-- | This function parses the cflags output of pkg-config. It only threats -I and -i flags special. The remaining
-- flags are just added to 'cflags'.
parseCompilerFlags :: T.Text -> Maybe PackageConfig
parseCompilerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where define = item defines . T.breakOn "=" <$> flag 'D'
        incPath = dir includePaths <$> flag 'I'
        other = item cflags <$> takeWhile1 (/= ' ')
        f = try incPath <|> try define <|> other

-- | This function parses the lflags output of pkg-config. It only threats -L and -l flags special. The remaining
-- flags are simply added to 'lflags'.
parseLinkerFlags :: T.Text -> Maybe PackageConfig
parseLinkerFlags = preview _Right . fmap mconcat . parseOnly (f `sepBy` many1 space)
  where lib = item libraries <$> flag 'l'
        libPath = dir libraryPaths <$> flag 'L'
        other = item cflags <$> takeWhile1 (/= ' ')
        f = try lib <|> try libPath <|> other

-- | Parse a command line flag
-- FIXME: This does not yet handle quotes in arguments
flag :: Char -> Parser T.Text
flag c = string ("-" <> T.singleton c) *> optional space *> takeWhile1 (/= ' ')

-- | Construct a 'PackageConfig' with a single directory list field.
dir :: Lens' PackageConfig [P.FilePath] -> T.Text -> PackageConfig
dir l = item l . P.fromText

-- | Construct a 'PackageConfig' with a single list field.
item :: Lens' PackageConfig [a] -> a -> PackageConfig
item l x = reviewEmpty l [x]
