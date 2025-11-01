{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Control.Monad (void)
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)

import           Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Control.Lens as L
import qualified Data.Aeson.Lens as AL
import qualified Data.Text.Lens as TL

import           Development.Shake
import           Development.Shake.Classes (Binary)
import           Development.Shake.FilePath ((</>), (-<.>), dropDirectory1)
import           Development.Shake.Forward (cacheAction, shakeArgsForward)
import           Slick (orgModeToHTML, convert, compileTemplate', substitute)

siteUrl, siteDescription, siteAuthor :: T.Text
siteUrl = "https://tuesta.me"
siteDescription = "Abstracción, composición, profundidad. El espacio donde mi reflexión se plasma en texto."
siteAuthor = "Tuesta"

outputFolder :: FilePath
outputFolder = "docs/"

newtype Tag = Tag {getTag :: String}
  deriving (Eq, Ord, Show)
  deriving newtype Binary

tagUrl :: String -> String
tagUrl t = "tag" </> toUrl t <> ".html"

instance ToJSON Tag where
  toJSON (Tag t) = A.object
    [ "tag" A..= if null t then "Todos" else t
    , "url" A..= if null t then "" else tagUrl t
    ]

instance FromJSON Tag where
  parseJSON v =
    let tag = v L.^. AL.key "tag" . AL._String . TL.unpacked
        url = v L.^. AL.key "url" . AL._String . TL.unpacked
     in pure $ Tag (if null url then "" else tag)

data MetaPost = MetaPost
  { title      :: String
  , date       :: (Int, Int, Int)
  } deriving (Generic, Eq, Ord, Show, Binary)

addZero :: String -> String
addZero n@[_] = '0':n
addZero n = n

yearMonthDay :: (Int, Int, Int) -> String
yearMonthDay (y, m, d) = show y<> "-" <> addZero (show m) <> "-" <> addZero (show d)

toSpanishDate :: (Int, Int, Int) -> String
toSpanishDate (y, m, d) = unwords [addZero (show d), toSpanish m, show y]
  where
    toSpanish i = (<> ".") . (!! pred i) $
      ["ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"]

instance ToJSON MetaPost where
  toJSON MetaPost{title, date} = A.object
    [ "title" A..= title
    , "date" A..= toSpanishDate date
    , "date-iso" A..= yearMonthDay date
    , "url" A..= ("posts/" <> toUrl title <> ".html")
    ]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

toYearMonthDay :: String -> (Int, Int, Int)
toYearMonthDay dateStr = case wordsWhen (== '-') dateStr of
  [y, m, d] -> (read y, read m, read d)
  _ -> error "toYearMonthDay: invalid format [year]-[month]-[day]"

dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (x:zs) (_:ys) = x : f zs ys
          f _ _ = []
         
instance FromJSON MetaPost where
  parseJSON v =
    let title = v L.^. AL.key "title" . AL._String . TL.unpacked
        dateStr = v L.^. AL.key "date" . AL._String . TL.unpacked
        cleanDate = takeWhile (/= ' ') . dropEnd 4 . drop 4
     in pure MetaPost{title, date = toYearMonthDay . cleanDate $ dateStr}
  
-- | Data for a blog post
data Post = Post
  { metaPost    :: MetaPost
  , subtitle    :: String
  , author      :: String
  , content     :: String
  , tags        :: [Tag]
  , description :: String
  } deriving (Generic, Eq, Ord, Show, Binary)

toUrl :: String -> String
toUrl = fmap ((\c -> fromMaybe c $ M.lookup c normalize) . toLower)
  where
    normalize = M.fromList
      [('á', 'a'),('é', 'e'),('í', 'i'),('ó', 'o'),('ú', 'u'),('ñ', 'n'),('ü', 'u'),(' ', '-'), ('\n', '-')]

instance ToJSON Post where
  toJSON Post{metaPost, subtitle, author, content, tags, description} = A.object
      [ "subtitle" A..= subtitle
      , "author" A..= author
      , "content" A..= content
      , "tags" A..= tags
      , "description" A..= description
      ] `union` toJSON metaPost

union :: A.Value -> A.Value -> A.Value
A.Object o1 `union` A.Object o2 = A.Object (o1 `A.union` o2)
_ `union` _ = error "union: only make union of two objects"
      
instance FromJSON Post where
  parseJSON v = do
    metaPost :: MetaPost <- A.parseJSON v
    let subtitle = v L.^. AL.key "subtitle" . AL._String . TL.unpacked
        author = v L.^. AL.key "author" . AL._String . TL.unpacked
        content = v L.^. AL.key "content" . AL._String . TL.unpacked
        keywords = v L.^. AL.key "keywords" . AL._String . TL.unpacked
        description = v L.^. AL.key "description" . AL._String . TL.unpacked
    pure Post{ metaPost, subtitle, author, content, description
             , tags = fmap Tag $ wordsWhen (==',') keywords
             }

data IndexTag = IndexTag
  { tagPosts :: [MetaPost]
  , allTags  :: S.Set Tag
  , tag      :: Tag
  } deriving (Show, Eq, Ord)

instance ToJSON IndexTag where
  toJSON IndexTag{tagPosts, allTags, tag} = A.object
    [ "url" A..= if null tg then (A.Bool False) else A.String (T.pack $ tagUrl tg)
    , "posts" A..= tagPosts
    , "allTags" A..= S.foldr (\t tsActive -> tagState t : tsActive) [] allTags
    ] where
      tg = getTag tag
      tagState :: Tag -> A.Value
      tagState t = A.object
        [ "state" A..= if tag == t then "active" else ("" :: String)
        ] `union` toJSON t

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

buildRules :: Action ()
buildRules = do
  (allTags, allPosts) <- buildPosts
  buildIndexs allTags allPosts
  copyStaticFiles

buildPosts :: Action (S.Set Tag, [MetaPost])
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.org"]
  foldr combine (S.singleton (Tag ""), mempty) <$> forP pPaths buildPost
    where combine (postTags, post) (allTags, posts) = (postTags <> allTags , post : posts)

withBaseUrl :: A.Value -> A.Value
withBaseUrl = AL._Object . L.at "siteUrl" L.?~ A.String siteUrl

buildPost :: FilePath -> Action (S.Set Tag, MetaPost)
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  postData <- orgModeToHTML . T.pack $ postContent

  post <- convert postData

  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      fullPostData = withBaseUrl $ toJSON post

  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData

  pure (S.fromList (tags post), metaPost post)

buildIndexs :: S.Set Tag -> [MetaPost] -> Action ()
buildIndexs tags posts = void $ forP (S.toList tags) (buildIndex tags posts)

withSiteMeta :: A.Value -> A.Value
withSiteMeta = withAuthor . withDescription . withBaseUrl
  where
    withDescription = AL._Object . L.at "siteDescription" L.?~ A.String siteDescription
    withAuthor = AL._Object . L.at "siteAuthor" L.?~ A.String siteAuthor

withTitle :: String -> A.Value -> A.Value
withTitle tag = AL._Object . L.at "siteTitle" L.?~ A.String
  (T.pack (if null tag then "Artículos" else tag) <> " :: Tuesta")

buildIndex :: S.Set Tag -> [MetaPost] -> Tag -> Action ()
buildIndex allTags tagMetaPosts (Tag tag) = do
  indexT <- compileTemplate' "site/templates/indexTag.html"
  let tagFilePath = if null tag then "index.html" else tagUrl tag
      indexTag = withTitle tag . withSiteMeta . A.toJSON $ IndexTag tagMetaPosts allTags (Tag tag)
      indexTagHTML = T.unpack $ substitute indexT (withBaseUrl indexTag)
  liftIO $ putStrLn tagFilePath
  writeFile' (outputFolder </> tagFilePath) indexTagHTML

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)
