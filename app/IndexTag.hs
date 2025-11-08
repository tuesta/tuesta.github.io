{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module IndexTag where

import           GHC.Generics (Generic)

import qualified Data.Set as S
import           Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Text as T

import           Post
import           Tag
import           Utils

data IndexTag = IndexTag
  { tagPosts :: [MetaPost]
  , allTags  :: S.Set Tag
  , tagIndex :: Tag
  } deriving (Generic, Show, Eq, Ord, FromJSON)

instance ToJSON IndexTag where
  toJSON IndexTag{tagPosts, allTags, tagIndex} = A.object
    [ "url" A..= tag (A.Bool False) (A.String . T.pack . tagUrl) tagIndex
    , "tagPosts" A..= tagPosts
    , "allTags" A..= S.foldr (\t tsActive -> tagState t : tsActive) [] allTags
    , "tagIndex" A..= tagIndex
    ] where
      tagState t = A.object
        [ "state" A..= (if tagIndex == t then "active" else ("" :: String))
        ] `union` toJSON t
