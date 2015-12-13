module Scrabble.ScrabbleAI
       (
         playScrabble,
         showScrabble
       ) where



import           Data.List               (intercalate)
import           Scrabble.Dictionary     (Dictionary, wordDict)
import           Scrabble.Scrabble       (Board, BonusBoard, Hand, Play, Tiles,
                                          playword, showPlay, showScrabbleBoard,
                                          tiles)
import           Scrabble.ScrabbleSolver (allPlays, bestPlayState, updateBoard)
import           Scrabble.Utils          (removeMany, sample)
import           System.Random           (StdGen, newStdGen)




showScrabble :: Dictionary -> Board -> BonusBoard -> IO String
showScrabble d b bB = fmap (showTransitions bB) . playScrabble d b $ bB



playScrabble :: Dictionary -> Board -> BonusBoard -> IO [(Play, Board)]
playScrabble d b bB = drawHand tiles [] >>= \(h, t) -> playScrabbleHelper d b bB h $ t



playScrabbleHelper :: Dictionary -> Board -> BonusBoard -> Hand -> Tiles -> IO [(Play, Board)]
playScrabbleHelper d b bB h t = case bestPlayState d b bB $ h of
                                 Nothing              -> return []
                                 Just (play, remHand) -> let board = play `updateBoard` b in
                                                          do
                                                            (hand, tiles) <- t `drawHand` remHand
                                                            rest <- playScrabbleHelper d board bB hand tiles
                                                            return $ (play, board): rest


drawHand :: Tiles -> Hand -> IO (Hand, Tiles)
drawHand tiles hand = newStdGen >>= \gen -> return . f . randomHand gen n $ tiles
    where f (h, t) = (hand ++ h, t)
          n        = min (7 - length hand) (length tiles)



randomHand :: StdGen -> Int -> Tiles -> (Hand, Tiles)
randomHand gen n tiles = let hand = sample gen n tiles in
                          (hand, tiles `removeMany` hand)




showTransitions :: BonusBoard -> [(Play, Board)] -> String
showTransitions bB sols = "\n\n" ++ (intercalate "\n\n" . map (showTransition bB) $ sols)



showTransition :: BonusBoard -> (Play, Board) -> String
showTransition bB (p, b) = showPlay p ++ "\n\n" ++ showScrabbleBoard b bB
