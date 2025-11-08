{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO en indexTag, no deberías poder hacer click en la etiqueta actual
module Main where

import           GHC.Generics (Generic)
import           Control.Monad (void)

import           Data.Aeson (ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Set as S
import qualified Data.Text as T

import           Control.Lens ((?~), at)
import           Data.Aeson.Lens (_Object)

import           Development.Shake
import           Development.Shake.FilePath ((</>), (-<.>), dropDirectory1)
import           Development.Shake.Forward (cacheAction, shakeArgsForward)
import           Slick (orgModeToHTML', compileTemplate', substitute)

import           Post
import           Tag
import           IndexTag

siteUrl, siteDescription, siteAuthor :: T.Text
siteUrl = "https://tuesta.me"
siteDescription = "Abstracción, composición, profundidad. El espacio donde mi reflexión se plasma en texto."
siteAuthor = "Tuesta"

withSiteUrl :: A.Value -> A.Value
withSiteUrl = _Object . at "siteUrl" ?~ A.String siteUrl

withTitle :: Tag -> A.Value -> A.Value
withTitle tg = _Object . at "siteTitle" ?~ A.String
  (T.pack (tag "Artículos" id tg) <> " :: Tuesta")

withSiteMeta :: A.Value -> A.Value
withSiteMeta = withSiteAuthor . withSiteDescription . withSiteUrl
  where withSiteDescription = _Object . at "siteDescription" ?~ A.String siteDescription
        withSiteAuthor = _Object . at "siteAuthor" ?~ A.String siteAuthor

outputFolder :: FilePath
outputFolder = "docs/"

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

buildRules :: Action ()
buildRules = do
  (allTags, allPosts) <- buildPosts
  buildIndexs allTags (metaPost <$> allPosts)
  buildFeed allPosts
  copyStaticFiles

buildPosts :: Action (S.Set Tag, [Post])
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.org"]
  foldr combine (S.singleton All, mempty) <$> forP pPaths buildPost
    where combine (postTags, post) (allTags, posts) = (postTags <> allTags , post : posts)

buildPost :: FilePath -> Action (S.Set Tag, Post)
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- T.pack <$> readFile' srcPath
  post <- process <$> orgModeToHTML' postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      fullPostData = withSiteUrl $ toJSON post
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  pure (S.fromList (tags post), post)

buildIndexs :: S.Set Tag -> [MetaPost] -> Action ()
buildIndexs tags posts = void $ forP (S.toList tags) (buildIndex tags posts)

buildIndex :: S.Set Tag -> [MetaPost] -> Tag -> Action ()
buildIndex allTags tagMetaPosts tg = do
  indexT <- compileTemplate' "site/templates/indexTag.html"
  let tagFilePath = tag "index.html" tagUrl tg
      indexTag = withTitle tg . withSiteMeta . A.toJSON $ IndexTag tagMetaPosts allTags tg
      indexTagHTML = T.unpack $ substitute indexT indexTag
  liftIO $ putStrLn tagFilePath
  writeFile' (outputFolder </> tagFilePath) indexTagHTML

data RSSData = RSSData
  { posts :: [Post]
  } deriving (Generic, Eq, Ord, Show, A.FromJSON, A.ToJSON)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  let rssData = RSSData {posts}
      fullRSSData = withSiteMeta $ toJSON rssData
  rssTempl <- compileTemplate' "site/templates/rss.xml"
  writeFile' (outputFolder </> "rss.xml") . T.unpack $ substitute rssTempl fullRSSData

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)
