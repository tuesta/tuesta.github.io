{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tag (Tag(All), mkTag, tag, fromTag, tagUrl) where

import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as A

import           Control.Lens ((^.))
import           Data.Aeson.Lens (_String, key)
import           Data.Text.Lens (unpacked)

import           Development.Shake.Classes (Binary)
import           Development.Shake.FilePath ((</>))

import           Utils

data Tag = All | Tag String
  deriving (Generic, Ord, Show, Binary)

mkTag :: String -> Tag
mkTag ""  = All
mkTag str = Tag str

tagUrl :: String -> String
tagUrl t = "tag" </> toUrl t <> ".html"

instance Eq Tag where
  t1 == t2 = fromTag t1 == fromTag t2

fromTag :: Tag -> String
fromTag All = ""
fromTag (Tag str) = str

tag :: b -> (String -> b) -> Tag -> b
tag b _ All = b
tag _ f (Tag a) = f a

instance ToJSON Tag where
  toJSON All = A.object
    [ "tag" A..= ("Todos" :: String)
    , "url" A..= ("" :: String)
    ]
  toJSON (Tag t) = A.object
    [ "tag" A..= t
    , "url" A..= tagUrl t
    ]

instance FromJSON Tag where
  parseJSON v =
    let t = v ^. key "tag" . _String . unpacked
        url = v ^. key "url" . _String . unpacked
     in pure $ if null url then All else mkTag t
