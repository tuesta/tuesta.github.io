{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Post where

import           GHC.Generics (Generic)

import           Data.Time
import           Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as A

import           Control.Lens ((^.), (^..))
import           Data.Aeson.Lens (_String, key, values)
import           Data.Text.Lens (unpacked)

import           Development.Shake.Classes (Binary)

import           Utils
import           Tag

isoToRfc822 :: String -> String
isoToRfc822 isoDate =
  formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (UTCTime day 0)
  where
    day :: Day = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" isoDate

type YearMonthDay = (Int, Int, Int)

data MetaPost = MetaPost
  { title        :: String
  , date         :: YearMonthDay
  } deriving (Generic, Eq, Ord, Show, Binary)

instance ToJSON MetaPost where
  toJSON MetaPost{title, date} = A.object
    [ "title" A..= title
    , "date" A..= toSpanishDate date
    , "date-iso" A..= dateISO
    , "date-rfc822" A..= isoToRfc822 dateISO
    , "url" A..= ("posts/" <> toUrl title <> ".html")
    ] where dateISO = yearMonthDay date

instance FromJSON MetaPost where
  parseJSON v =
    let title = v ^. key "title" . _String . unpacked
        date = toYearMonthDay $ v ^. key "date-iso" . _String . unpacked
     in pure MetaPost{ title, date }

data Post = Post
  { metaPost    :: MetaPost
  , subtitle    :: String
  , author      :: String
  , content     :: String
  , description :: String
  , tags        :: [Tag]
  } deriving (Generic, Eq, Ord, Show, Binary)

instance ToJSON Post where
  toJSON Post{metaPost, subtitle, author, content, tags, description} = A.object
      [ "subtitle" A..= subtitle
      , "author" A..= author
      , "content" A..= content
      , "tags" A..= tags
      , "description" A..= description
      ] `union` toJSON metaPost

instance FromJSON Post where
  parseJSON v = do
    metaPost <- A.parseJSON v
    let subtitle = v ^. key "subtitle" . _String . unpacked
        author = v ^. key "author" . _String . unpacked
        content = v ^. key "content" . _String . unpacked
        description = v ^. key "description" . _String . unpacked
    tags <- mapM A.parseJSON (v ^.. key "tags" . values)
    pure Post{metaPost, subtitle, author, content, description, tags}

data PostRAW = PostRAW
  { author      :: String
  , title       :: String
  , subtitle    :: String
  , content     :: String
  , date        :: String
  , description :: String
  , lang        :: String
  , keywords    :: String
  } deriving (Generic, Eq, Ord, Show, Binary, FromJSON, ToJSON)

toYearMonthDay :: String -> (Int, Int, Int)
toYearMonthDay dateStr = case wordsWhen (== '-') dateStr of
  [y, m, d] -> (read y, read m, read d)
  _ -> error "toYearMonthDay: invalid format [year]-[month]-[day]"

process :: PostRAW -> Post
process PostRAW{author, title, subtitle, content, description, keywords, date} =
  let tags = mkTag <$> wordsWhen (== ',') keywords
   in Post{ author, content, description, tags, subtitle
          , metaPost = MetaPost{title, date = toYearMonthDay date}
          }
