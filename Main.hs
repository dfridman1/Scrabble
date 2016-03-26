module Main where




import           Scrabble.Dictionary    (Dictionary, makeDictionary)
import           Scrabble.Scrabble      (fromBonusBoard)
import           Scrabble.ScrabbleAI    (showScrabbleIO)
import           Scrabble.ScrabbleParse (readBonusBoard)



filename :: FilePath
filename = "resources/4kwords.txt"




readDictionary :: IO Dictionary
readDictionary = readFile filename >>= return . makeDictionary



bonusBoardStr :: String
bonusBoardStr = unlines ["|||||||||||||||||",
                         "|3..:...3...:..3|",
                         "|.2...;...;...2.|",
                         "|..2...:.:...2..|",
                         "|:..2...:...2..:|",
                         "|....2.....2....|",
                         "|.;...;...;...;.|",
                         "|..:...:.:...:..|",
                         "|3..:...*...:..3|",
                         "|..:...:.:...:..|",
                         "|.;...;...;...;.|",
                         "|....2.....2....|",
                         "|:..2...:...2..:|",
                         "|..2...:.:...2..|",
                         "|.2...;...;...2.|",
                         "|3..:...3...:..3|",
                         "|||||||||||||||||"]



main :: IO ()
main = do
  d <- readDictionary
  case readBonusBoard bonusBoardStr of
   Left err -> putStrLn err
   Right b  -> let board = fromBonusBoard b in
                showScrabbleIO d board b >>= putStrLn
