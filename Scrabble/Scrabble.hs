{-# LANGUAGE NamedFieldPuns #-}


module Scrabble.Scrabble
       (
         Square(..),
         BonusSquare(..),
         isLetter,
         isEmpty,
         isAnchor,
         toWord,
         fromWord,
         Board,
         BonusBoard,
         letters,
         Hand,
         Direction(..),
         otherDirection,
         Play(..),
         PlayState,
         Pos,
         tiles,
         Tiles,
         Score,
         possibilities,
         points,
         showScrabbleBoard,
         fromBonusBoard,
         showPlay
       ) where




import           Data.List           (intercalate, intersperse)
import           Prelude             hiding (Word)
import           Scrabble.Dictionary (Word)
import           Scrabble.Utils      (merge)




data Square = Border
            | Empty
            | Letter Char
            | Anchor [Char]
              deriving Show



isLetter :: Square -> Bool
isLetter (Letter _) = True
isLetter _          = False


isEmpty :: Square -> Bool
isEmpty (Anchor _) = True
isEmpty Empty      = True
isEmpty _          = False


isAnchor :: Square -> Bool
isAnchor (Anchor _) = True
isAnchor _          = False



toWord :: [Square] -> Word
toWord = map toChar


fromWord :: Word -> [Square]
fromWord = map Letter


toChar :: Square -> Char
toChar (Letter c) = c
toChar _          = error "Character expected"



data BonusSquare = BonusBorder
                 | BonusEmpty
                 | Start
                 | DoubleWord
                 | TripleWord
                 | DoubleLetter
                 | TripleLetter
                   deriving Show


type Board      = [[Square]]
type BonusBoard = [[BonusSquare]]


type Hand  = String
type Tiles = String


letters :: [Char]
letters = ['A'..'Z']


tiles :: [Char]
tiles = (2 `replicate` blank) ++ (concat $ zipWith (flip replicate) ['A'..'Z'] [9, 2, 2, 4, 12, 2, 3,
                                                                                2, 9, 1, 1, 4, 2, 6,
                                                                                8, 2, 1, 6, 4, 6, 4,
                                                                                2, 2, 1, 2, 1])


blank :: Char
blank = ' '


isBlank :: Char -> Bool
isBlank = (== blank)



possibilities :: Char -> [Char]
possibilities c | c `elem` letters = [c]
                | isBlank c        = letters
                | otherwise        = []



points :: [(Char, Score)]
points = ['A'..'Z'] `zip` [1, 3, 3, 2, 1, 4, 2, 4, 1,
                           8, 5, 1, 3, 1, 1, 3, 10, 1,
                           1, 1, 1, 4, 4, 8, 4, 10]




showMatrix :: (a -> Char) -> [[a]] -> String
showMatrix f = intercalate "\n" . map (intersperse ' ' . map f)




showBoard :: Board -> String
showBoard = showMatrix showSquare



showBonusBoard :: BonusBoard -> String
showBonusBoard = showMatrix showBonusSquare



showSquare :: Square -> Char
showSquare sq = case sq of
                 Border   -> '|'
                 Empty    -> '.'
                 Letter c -> c
                 Anchor _ -> '*'



showBonusSquare :: BonusSquare -> Char
showBonusSquare sq = case sq of
                      BonusBorder  -> '|'
                      BonusEmpty   -> '.'
                      Start        -> '*'
                      DoubleWord   -> '2'
                      TripleWord   -> '3'
                      DoubleLetter -> ':'
                      TripleLetter -> ';'




fromBonusBoard :: BonusBoard -> Board
fromBonusBoard = (map . map) fromBonusSquare



fromBonusSquare :: BonusSquare -> Square
fromBonusSquare sq = case sq of
                      BonusBorder -> Border
                      Start       -> Anchor letters
                      _           -> Empty




showScrabbleBoard :: Board -> BonusBoard -> String
showScrabbleBoard b = showMatrix showScrabbleSquare . merge b



showScrabbleSquare :: (Square, BonusSquare) -> Char
showScrabbleSquare p = case p of
                        (Letter c, _) -> c
                        (_, sq)       -> showBonusSquare sq





type Pos   = (Int, Int)
type Score = Int


data Direction = Down | Across deriving Show


otherDirection :: Direction -> Direction
otherDirection Across = Down
otherDirection Down   = Across



data Play = Play { playpos   :: Pos,
                   playdir   :: Direction,
                   playword  :: Word,
                   playscore :: Score } deriving Show





type PlayState = (Play, Hand)




showPlay :: Play -> String
showPlay (Play { playdir, playword, playpos, playscore }) = (show playdir ++ " (pos = " ++ show (oneIndexed playpos)
                                                             ++ ") word " ++ show playword ++
                                                             " scored " ++ show playscore ++ " points!")


oneIndexed :: Pos -> Pos
oneIndexed (x, y) = (x + 1, y + 1)
