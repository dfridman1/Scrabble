module Scrabble.Dictionary
       (
         Word,
         Prefix,
         WordDict,
         PrefixDict,
         Dictionary,
         wordDict,
         prefixDict,
         makeDictionary,
         contains
       ) where



import           Data.Char (toUpper)
import           Data.List (inits)
import qualified Data.Set  as Set
import           Prelude   hiding (Word)



type Word   = String
type Prefix = String

type WordDict   = Set.Set Word
type PrefixDict = Set.Set Prefix

type Dictionary = (WordDict, PrefixDict)



wordDict :: Dictionary -> WordDict
wordDict = fst



prefixDict :: Dictionary -> PrefixDict
prefixDict = snd



makeDictionary :: String -> Dictionary
makeDictionary ws = (makeWordDict xs, makePrefixDict xs)
    where xs = map (map toUpper) . words $ ws



makeWordDict :: [Word] -> WordDict
makeWordDict = Set.fromList



makePrefixDict :: [Word] -> PrefixDict
makePrefixDict = Set.fromList . concatMap prefixes



prefixes :: Word -> [Prefix]
prefixes = init . inits


contains :: Ord a => Set.Set a -> a -> Bool
contains = flip Set.member
