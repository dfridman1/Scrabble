module Scrabble.ScrabbleParse
       (
         readBoard,
         readBonusBoard
       ) where



import           Data.Char         (toUpper)
import           Scrabble.Scrabble (Board, BonusBoard, BonusSquare (..),
                                    Square (..), letters)



type ErrorMsg = String



readMatrix :: (Char -> Either ErrorMsg a) -> String -> Either ErrorMsg [[a]]
readMatrix f s = (checkDimensions . lines $ s) >>= (mapM . mapM) f



checkDimensions :: [[a]] -> Either ErrorMsg [[a]]
checkDimensions l@(x: xs) | all (\y -> length y == length x) xs = Right l
                          | otherwise                           = Left "error: non-rectangular board"
checkDimensions []                                              = Right []




readBoard :: String -> Either ErrorMsg Board
readBoard = readMatrix readSquare



readSquare :: Char -> Either ErrorMsg Square
readSquare c = case toUpper c of
                '|' -> Right Border
                '.' -> Right Empty
                ch  -> if ch `elem` letters then Right $ Letter ch else Left $ "error: invalid character " ++ [c]



readBonusBoard :: String -> Either ErrorMsg BonusBoard
readBonusBoard = readMatrix readBonusSquare



readBonusSquare :: Char -> Either ErrorMsg BonusSquare
readBonusSquare c = case c of
                     '|' -> Right BonusBorder
                     '.' -> Right BonusEmpty
                     '*' -> Right Start
                     '2' -> Right DoubleWord
                     '3' -> Right TripleWord
                     ':' -> Right DoubleLetter
                     ';' -> Right TripleLetter
                     _   -> Left $ "error: invalid character " ++ [c]
