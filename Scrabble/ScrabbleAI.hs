module Scrabble.ScrabbleAI
       (
         playScrabble,
         playScrabbleIO,
         showScrabble,
         showScrabbleIO
       ) where



import           Data.List               (intercalate)
import           Scrabble.Dictionary     (Dictionary, wordDict)
import           Scrabble.Scrabble       (Board, BonusBoard, Hand, Play, Tiles,
                                          playword, showPlay, showScrabbleBoard,
                                          tiles)
import           Scrabble.ScrabbleSolver (allPlays, bestPlayState, updateBoard)
import           Scrabble.Utils          (removeMany, sample)
import           System.Random           (StdGen, newStdGen)





showScrabbleIO :: Dictionary -> Board -> BonusBoard -> IO String
showScrabbleIO d b bB = newStdGen >>= \g -> return $ showScrabble g d b $ bB


showScrabble :: StdGen -> Dictionary -> Board -> BonusBoard -> String
showScrabble g d b bB = showTransitions bB . playScrabble g d b $ bB


playScrabbleIO :: Dictionary -> Board -> BonusBoard -> IO [(Play, Board)]
playScrabbleIO d b bB = newStdGen >>= \g -> return $ playScrabble g d b bB


playScrabble :: StdGen -> Dictionary -> Board -> BonusBoard -> [(Play, Board)]
playScrabble g d b bB = let ((hand, t), gen) = initialHand g tiles in
                         playScrabbleHelper gen d b bB hand t


playScrabbleHelper :: StdGen -> Dictionary -> Board -> BonusBoard -> Hand -> Tiles -> [(Play, Board)]
playScrabbleHelper g d b bB h t = case bestPlayState d b bB $ h of
                                   Nothing -> []
                                   Just (play, remHand) -> let board                = play `updateBoard` b
                                                               ((hand, tiles), gen) = drawHand g t remHand in
                                                            (play, board): playScrabbleHelper gen d board bB hand tiles



initialHand :: StdGen -> Tiles -> ((Hand, Tiles), StdGen)
initialHand g t = drawHand g t []



drawHand :: StdGen -> Tiles -> Hand -> ((Hand, Tiles), StdGen)
drawHand gen tiles hand = f $ randomHand gen n tiles
    where f ((h, t), newgen) = ((hand ++ h, t), newgen)
          n                  = min (7 - length hand) (length tiles)


randomHand :: StdGen -> Int -> Tiles -> ((Hand, Tiles), StdGen)
randomHand gen n tiles = let (hand, newgen) = sample gen n tiles in
                          ((hand, tiles `removeMany` hand), newgen)




showTransitions :: BonusBoard -> [(Play, Board)] -> String
showTransitions bB sols = "\n\n" ++ (intercalate "\n\n" . map (showTransition bB) $ sols)



showTransition :: BonusBoard -> (Play, Board) -> String
showTransition bB (p, b) = showPlay p ++ "\n\n" ++ showScrabbleBoard b bB
