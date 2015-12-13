{-# LANGUAGE NamedFieldPuns #-}



module Scrabble.ScrabbleSolver
       (
         allPlays,
         bestPlay,
         bestPlayState,
         updateBoard
       ) where



import           Data.List           (maximumBy)
import           Data.List           (intercalate)
import           Data.Maybe          (fromJust)
import           Prelude             hiding (Word)
import           Scrabble.Dictionary (Dictionary, Prefix, PrefixDict, Word,
                                      WordDict, contains, prefixDict, wordDict)
import           Scrabble.Scrabble   (Board, BonusBoard, BonusSquare (..),
                                      Direction (..), Hand, Play (..),
                                      PlayState, Pos, Score, Square (..), Tiles,
                                      fromWord, isAnchor, isEmpty, isLetter,
                                      letters, otherDirection, points,
                                      possibilities, showScrabbleBoard, tiles,
                                      toWord)
import           Scrabble.Utils      (horizontalNeighbors, horizontalSplits,
                                      merge, removeOne, swap, transpose,
                                      verticalSplits)







updateBoard :: Play -> Board -> Board
updateBoard p b = case playdir p of
                   Across -> updateBoardHor pos word b
                   Down   -> transpose . updateBoardHor (swap pos) word . transpose $ b
    where pos  = playpos p
          word = playword p




updateBoardHor :: Pos -> Word -> Board -> Board
updateBoardHor (r, c) w b = iter 0 b
    where iter row (x: xs) | row == r = ((take c x) ++ (fromWord w) ++ (drop (c + length w) x)): iter (row + 1) xs
                           | otherwise = x: iter (row + 1) xs
          iter _ [] = []



bestPlay :: Dictionary -> Board -> BonusBoard -> Hand -> Maybe Play
bestPlay d b bB = fmap fst . bestPlayState d b bB



bestPlayState :: Dictionary -> Board -> BonusBoard -> Hand -> Maybe PlayState
bestPlayState d b bB = bestPlayStateHelper . allPlays d b bB



bestPlayStateHelper :: [PlayState] -> Maybe PlayState
bestPlayStateHelper [] = Nothing
bestPlayStateHelper ps = Just . maximumBy cmpFn $ ps
    where cmpFn p1 p2 = (g p1) `compare` (g p2)
          g           = playscore . fst





allPlays :: Dictionary -> Board -> BonusBoard -> Hand -> [PlayState]
allPlays d b bB h = rowPlays d b bB h ++ columnPlays d b bB h




rowPlays :: Dictionary -> Board -> BonusBoard -> Hand -> [PlayState]
rowPlays d b bB h = concatMap f . zip [0..] . rowPlaysHelper d board $ h
    where f (r, xs) = [(makePlay (wordDict d) b bB (r, c) Across w, hand) | (c, w, hand) <- concat xs]
          board     = setAnchors (wordDict d) b




makePlay :: WordDict -> Board -> BonusBoard -> Pos -> Direction -> Word -> Play
makePlay d b bB pos dir w = play

    where score = calculateScore d b bB play
          play  = Play { playpos   = pos,
                         playdir   = dir,
                         playword  = w,
                         playscore = score }




type Column   = Int
type RowPlays = [(Column, Word, Hand)]




rowPlaysHelper :: Dictionary -> Board -> Hand -> [[RowPlays]]
rowPlaysHelper d b h = (map . map) f . merge b . horizontalSplits $ b
    where f (sq, (pref, suff)) | not $ isAnchor sq = []
                               | otherwise         = [(len - length pre, w, hand) | (pre, hs) <- findPrefixes (prefixDict d) pref h,
                                                      (w, hand) <- addSuffixes d suff hs pre]
              where len = length pref





columnPlays :: Dictionary -> Board -> BonusBoard -> Hand -> [PlayState]
columnPlays d b bB h = map f $ rowPlays d (transpose b) (transpose bB) h
    where f (play, hand) = (rotatePlay play, hand)




rotatePlay :: Play -> Play
rotatePlay p = p { playpos = swap $ playpos p, playdir = otherDirection $ playdir p }





type PrefixSq = [Square]



data CrossStruct a = CrossStruct { structprefix :: [a],
                                   structsquare :: a,
                                   structsuffix :: [a] } deriving Show



flattenStructure :: CrossStruct a -> [a]
flattenStructure s = (structprefix s) ++ (structsquare s): structsuffix s



type EastWest = (Maybe Square, Maybe Square)



letterEastWest :: EastWest -> Bool
letterEastWest (e, w) = (maybe False isLetter e) || (maybe False isLetter w)


type AnchorState = (EastWest, CrossStruct Square)


setAnchors :: WordDict -> Board -> Board
setAnchors d = (map . map) (toAnchor d) . boardAnchorStates



boardAnchorStates :: Board -> [[AnchorState]]
boardAnchorStates b = merge (horizontalNeighbors b) $ boardStructures isLetter b



toAnchor :: WordDict -> AnchorState -> Square
toAnchor d (eastwest, struct) | not $ isEmpty sq        = sq
                              | validStructure struct   = Anchor $ structChars d struct
                              | letterEastWest eastwest = Anchor letters
                              | otherwise               = sq
    where sq = structsquare struct



structChars :: WordDict -> CrossStruct Square -> [Char]
structChars d s = [c | c <- letters,
                   let word = (toWord $ structprefix s) ++ [c] ++ (toWord $ structsuffix s),
                       d `contains` word]




boardStructures :: (a -> Bool) -> [[a]] -> [[CrossStruct a]]
boardStructures p b = zipWith (zipWith f) b . verticalSplits $ b
    where f sq (pre, suff) = makeStructure p sq pre $ tail suff





validStructure :: CrossStruct a -> Bool
validStructure s = not (null (structprefix s) && null (structsuffix s))



makeStructure :: (a -> Bool) -> a -> [a] -> [a] -> CrossStruct a
makeStructure p sq xs ys = CrossStruct pre sq suff
    where pre  = reverse . takeWhile p . reverse $ xs
          suff = takeWhile p ys








findPrefixes :: PrefixDict -> [Square] -> Hand -> [(Prefix, Hand)]
findPrefixes d pref h = case legalPrefix pref of
                        Left maxsize -> filter (\(p, _) -> length p <= maxsize) $ prefixes d h
                        Right pre    -> [(pre, h)]



prefixes :: PrefixDict -> Hand -> [(Prefix, Hand)]
prefixes d h = extendPrefix d h []



extendPrefix :: PrefixDict -> Hand -> Prefix -> [(Prefix, Hand)]
extendPrefix d h pre | d `contains` pre = (pre, h): rest
                     | otherwise        = []

    where rest = concat [extendPrefix d (h `removeOne` c) (pre ++ [char]) |
                         c <- h, char <- possibilities c]





legalPrefix :: PrefixSq -> Either Int Prefix
legalPrefix pre = case takeWhile isLetter revPre of
                   [] -> Left . length . takeWhile (\sq -> isEmpty sq && (not $ isAnchor sq)) $ revPre
                   xs -> Right . toWord . reverse $ xs
    where revPre = reverse pre





addSuffixes :: Dictionary -> [Square] -> Hand -> Prefix -> [(Word, Hand)]
addSuffixes d suff h pre = addSuffixesHelper d suff h pre False



addSuffixesHelper :: Dictionary -> [Square] -> Hand -> Prefix -> Bool -> [(Word, Hand)]
addSuffixesHelper d suff h pre anchored | anchored && (wordDict d) `contains` pre && notTouchingLetter = (pre, h): rest
                                        | otherwise                                                    = rest
    where rest | not $ (prefixDict d) `contains` pre = []
               | otherwise = case suff of
                              (x: xs) -> case x of
                                          Empty        -> concat [addSuffixesHelper d xs (h `removeOne` c) (pre ++ [char]) anchored |
                                                                  c <- h, char <- possibilities c]
                                          Anchor chars -> concat [addSuffixesHelper d xs (h `removeOne` c) (pre ++ [char]) True |
                                                                  c <- h, char <- possibilities c, char `elem` chars]
                                          Letter c     -> addSuffixesHelper d xs h (pre ++ [c]) anchored
                                          Border       -> []
                              []      -> []

          notTouchingLetter = case suff of
                               (sq: _) -> not $ isLetter sq
                               []      -> True






calculateScore :: WordDict -> Board -> BonusBoard -> Play -> Score
calculateScore d b bB p = wordScore (map structsquare structures) word + crosswordsScore
    where structures      = extract b bB p
          validStructures = filter (validStructure . fst) . filter (isEmpty . fst . structsquare . fst) . zip structures $ word
          crosswords      = map (\(s, c) -> (flattenStructure s, c)) validStructures
          word            = playword p
          crosswordsScore = sum . map (uncurry (crosswordScore d)) $ crosswords



crosswordScore :: WordDict -> [(Square, BonusSquare)] -> Char -> Score
crosswordScore d xs c = if d `contains` word then score else 0
    where word  = toWord . replaceEmpty (Letter c) . map fst $ xs
          score = wordScore xs word


replaceEmpty :: Square -> [Square] -> [Square]
replaceEmpty x = map (\sq -> if isEmpty sq then x else sq)



crosswordToWord :: [(Square, BonusSquare)] -> Word
crosswordToWord = toWord . map fst




makeCrossword :: CrossStruct (Square, BonusSquare) -> Char -> [(Square, BonusSquare)]
makeCrossword s c = flattenStructure newS
    where newS = case structsquare s of
                  (_, bsq) -> s { structsquare = (Letter c, bsq) }



extract :: Board -> BonusBoard -> Play -> [CrossStruct (Square, BonusSquare)]
extract b bB p = case playdir p of
                  Across -> extractHor (f $ merge b bB) pos (length word)
                  Down   -> extractHor (f $ merge (transpose b) (transpose bB)) (swap pos) (length word)
    where word = playword p
          pos  = playpos p
          f    = boardStructures (isLetter . fst)



extractHor :: [[a]] -> Pos -> Int -> [a]
extractHor l (r, c) n = iter 0 l
    where iter row (x: xs) | r == row  = take n $ drop c x
                           | otherwise = iter (row + 1) xs
          iter _ [] = []




wordScore :: [(Square, BonusSquare)] -> Word -> Score
wordScore xs w = (wordMultiplier xs) * sum (map (\(p, c) -> charScore p c) (xs `zip` w))



wordMultiplier :: [(Square, BonusSquare)] -> Int
wordMultiplier = product . map f
    where f p = case p of
                 (Letter _, _)   -> 1
                 (_, DoubleWord) -> 2
                 (_, TripleWord) -> 3
                 _               -> 1



charScore :: (Square, BonusSquare) -> Char -> Score
charScore p c = (letterMultiplier p) * (fromJust $ lookup c points)



letterMultiplier :: (Square, BonusSquare) -> Int
letterMultiplier p = case p of
                      (Letter _, _)     -> 1
                      (_, DoubleLetter) -> 2
                      (_, TripleLetter) -> 3
                      _                 -> 1
