module Utils where

import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)

import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A

toUrl :: String -> String
toUrl = fmap ((\c -> fromMaybe c $ M.lookup c normalize) . toLower)
  where
    normalize = M.fromList
      [('á', 'a'),('é', 'e'),('í', 'i'),('ó', 'o'),('ú', 'u'),('ñ', 'n'),('ü', 'u'),(' ', '-'), ('\n', '-')]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

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

union :: A.Value -> A.Value -> A.Value
A.Object o1 `union` A.Object o2 = A.Object (o1 `A.union` o2)
_ `union` _ = error "union: only make union of two objects"
