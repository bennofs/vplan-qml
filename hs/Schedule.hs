{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Schedule where

import Data.VPlan
import Control.Lens
import Control.Monad
import Control.Exception.Lens
import Data.Maybe
import Data.Aeson
import Data.Semigroup
import Data.Group
import Data.Data
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Storable
import Data.Void
import Data.Functor
import Data.Bifunctor
import System.IO.Error
import Control.Applicative
import qualified Data.ByteString.Lazy as LBS

--------------------------------------------------------------------------------
-- Timetable type
--
-- We don't use a type synonym / newtype here to avoid a bug in GHC before version
-- 7.8.  Those versions generate a warning.
data Timetable = Timetable { getTimetable :: UScheduleVoidConfig LessonLocation Int }

timetable :: Iso' Timetable (UScheduleVoidConfig LessonLocation Int)
timetable = iso getTimetable Timetable

newtype LessonLocation = LessonLocation { getLessonLocation ::  (WeekDate, DiscreteTime) }
  deriving (Eq, Ord, Monoid, Semigroup, Group)

instance Enum LessonLocation where
  toEnum x = LessonLocation (mempty, _DiscreteTime # toInteger x)
  fromEnum = fromInteger . view lessonNr

_LessonLocation :: Iso' LessonLocation (WeekDate, DiscreteTime)
_LessonLocation = iso getLessonLocation LessonLocation

lessonLocation :: Integer -> WeekDay -> Integer -> LessonLocation
lessonLocation w d l = LessonLocation (WeekDate w d, _DiscreteTime # l)

lessonIx :: Integer -> WeekDay -> Integer -> Traversal' Timetable Int
lessonIx w d l = timetable . ix (lessonLocation w d l)

lessonNr :: Lens' LessonLocation Integer
lessonNr = _LessonLocation . _2 . _DiscreteTime

lessonDate :: Lens' LessonLocation WeekDate
lessonDate = _LessonLocation . _1

instance HasWeek LessonLocation Integer where week = lessonDate . week
instance HasDay  LessonLocation WeekDay where day  = lessonDate . day

instance FromJSON LessonLocation where
  parseJSON = withObject "Object" $ \o -> LessonLocation <$> do (,) <$> parseJSON (Object $ sans "nr" o) <*> (review _DiscreteTime <$> o .: "nr")

instance ToJSON LessonLocation where
  toJSON (LessonLocation (date, nr)) = let (Object o) = toJSON date in Object $ o & at "nr" ?~ Number (fromInteger $ nr ^. _DiscreteTime)

--------------------------------------------------------------------------------
-- C interface
imaxOnOf :: (Bifunctor p, Limited (p i' v), i' ~ Index (p i' v)) => Iso' i i' -> Lens' a i -> p a v -> Maybe i
imaxOnOf i j = fmap (review i) . imax . first (view $ j . i)

hs_scheduleMaxLessons :: StablePtr Timetable -> IO CInt
hs_scheduleMaxLessons ptr = fromInteger . fromMaybe 0 . imaxOnOf (from _DiscreteTime) lessonNr . view timetable <$> deRefStablePtr ptr

hs_scheduleLoadFile :: CWString -> Int -> Ptr CWString -> IO (StablePtr Timetable)
hs_scheduleLoadFile cwstr strlen cwerror = handling _IOException (newCWString . handle >=> poke cwerror >=> const (newStablePtr $ timetable # blank)) $ do
  fileName <- peekCWStringLen (cwstr,strlen)
  result <- eitherDecode <$> LBS.readFile fileName
  case result of
    Left err -> do
      newCWString err >>= poke cwerror
      newStablePtr $ timetable # blank
    Right t -> do
      newStablePtr $ timetable # t
  where handle err
          | isDoesNotExistError err = "File does not exist"
          | isPermissionError err   = "Permission denied"
          | isIllegalOperation err  = "Illegal operation"

hs_scheduleIndex :: StablePtr Timetable -> CInt -> CInt -> CInt -> IO CInt
hs_scheduleIndex sp w d l = do
  s <- deRefStablePtr sp
  return $ s ^?! lessonIx (toInteger w) (_WeekDay # fromIntegral d) (toInteger l) . enum

foreign export ccall hs_scheduleIndex :: StablePtr Timetable -> CInt -> CInt -> CInt -> IO CInt
foreign export ccall hs_scheduleMaxLessons :: StablePtr Timetable -> IO CInt
foreign export ccall hs_scheduleLoadFile :: CWString -> Int -> Ptr CWString -> IO (StablePtr Timetable)
