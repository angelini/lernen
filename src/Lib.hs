module Lib
  ( knownWords,
    learningWords,
    renderCorrect,
    renderOrder,
    renderShuffled,
    renderKeys,
    shuffle,
    size,
    orderedKeys,
    validate,
    parseFile,
    Key(..),
    Keys,
    Order,
    TextPair,
    ShuffledWords,
    Words,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified System.Random.Shuffle as Shuffle
import qualified Text.XML as XML
import Text.XML.Cursor (($//), (&/))
import qualified Text.XML.Cursor as Cursor
import Prelude hiding (Word)

range :: Map.Map a b -> [Int]
range map = take (Map.size map) [0 ..]

newtype Index = Index Int deriving (Eq, Ord, Show)
type Order = [Index]

newtype Key = Key Int deriving (Eq, Ord, Show)
type Keys = [Key]

indexFromKey :: Keys -> Key -> Index
indexFromKey keys key = case List.find (\(_, k) -> k == key) (zip [0 ..] keys) of
  Just (idx, _) -> Index idx
  Nothing -> undefined

type Word = T.Text
type Words = Map.Map Index Word

wordsFromText :: T.Text -> Words
wordsFromText text = Map.fromList $ zip (map Index [0 ..]) (T.splitOn " " text)

lookupIndex :: Words -> Index -> T.Text
lookupIndex words index = Maybe.fromMaybe "" $ Map.lookup index words

renderOrder :: Words -> Order -> T.Text
renderOrder words order = T.unwords $ map (lookupIndex words) order

renderCorrect :: Words -> T.Text
renderCorrect words = renderOrder words (map Index $ range words)

data ShuffledWords = ShuffledWords Words Keys deriving (Eq, Show)

shuffle :: Words -> IO ShuffledWords
shuffle words = do
  key_values <- Shuffle.shuffleM $ range words
  let keys = map Key key_values
  return $ ShuffledWords words keys

size :: ShuffledWords -> Int
size (ShuffledWords words _)= length words

orderedKeys :: ShuffledWords -> Keys
orderedKeys shuffled = map Key [0 .. size shuffled - 1]

validate :: ShuffledWords -> Keys -> Bool
validate (ShuffledWords _ expected) actual = actual == expected

lookupKey :: ShuffledWords -> Key -> T.Text
lookupKey (ShuffledWords words keys) key =
  let index = indexFromKey keys key
  in lookupIndex words index

renderShuffled :: ShuffledWords -> Keys -> T.Text
renderShuffled (ShuffledWords words keys) attempt =
  let order = map (indexFromKey keys) attempt
  in renderOrder words order

renderKeys :: ShuffledWords -> Keys -> T.Text
renderKeys shuffled = foldl addKey ""
  where
    keyToText (Key k) = T.pack $ show k
    wordSpaces key = T.pack $ replicate (T.length (lookupKey shuffled key)) ' '
    addKey acc key = acc <> keyToText key <> wordSpaces key

type TextPair = (Words, Words)

knownWords :: TextPair -> Words
knownWords (a, _) = a

learningWords :: TextPair -> Words
learningWords (_, b) = b

pairFromList :: [T.Text] -> TextPair
pairFromList [a, b] = (wordsFromText a, wordsFromText b)

parseFile :: FilePath -> IO [TextPair]
parseFile path = do
  doc <- XML.readFile XML.def path
  let units = Cursor.fromDocument doc $// Cursor.element "tu"
  return $ map findPair units
  where
    findPair cursor = pairFromList . reverse $ cursor $// Cursor.element "seg" &/ Cursor.content
