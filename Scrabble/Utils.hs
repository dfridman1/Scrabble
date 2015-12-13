module Scrabble.Utils
       (
         East,
         West,
         South,
         North,
         northN,
         southN,
         eastN,
         westN,
         Neighbors,
         horizontalNeighbors,
         verticalNeighbors,
         neighbors,
         verticalSplits,
         horizontalSplits,
         removeOne,
         removeMany,
         merge,
         merge3,
         transpose,
         swap,
         sample
       ) where



import           Control.Monad.State.Lazy (evalState)
import           Data.List                (intercalate)
import           Data.Random              hiding (sample)
import qualified Data.Random.Extras       as R
import           System.Random




splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [0..length xs - 1]]


verticalSplits :: [[a]] -> [[([a], [a])]]
verticalSplits = transpose . horizontalSplits . transpose


horizontalSplits :: [[a]] -> [[([a], [a])]]
horizontalSplits = map splits



type East a  = Maybe a
type West a  = Maybe a
type South a = Maybe a
type North a = Maybe a


type Neighbors a = (North a, South a, East a, West a)


neighbors :: [[a]] -> [[Neighbors a]]
neighbors xs = zipWith (zipWith combine) (horizontalNeighbors xs) (verticalNeighbors xs)
    where combine (e, w) (n, s) = (n, s, e, w)


northN :: Neighbors a -> North a
northN (n, _, _, _) = n


southN :: Neighbors a -> South a
southN (_, s, _, _) = s


eastN :: Neighbors a -> East a
eastN (_, _, e, _) = e


westN :: Neighbors a -> West a
westN (_, _, _, w) = w


horizontalNeighbors :: [[a]] -> [[(East a, West a)]]
horizontalNeighbors = (map . map) f . horizontalSplits
    where f (pre, suff) = (eastNeighbor pre, westNeighbor suff)


verticalNeighbors :: [[a]] -> [[(North a, South a)]]
verticalNeighbors = transpose . horizontalNeighbors . transpose


eastNeighbor :: [a] -> East a
eastNeighbor [] = Nothing
eastNeighbor xs = Just $ last xs


westNeighbor :: [a] -> West a
westNeighbor (_: y: _) = Just y
westNeighbor _         = Nothing


transpose :: [[a]] -> [[a]]
transpose []      = []
transpose ([]: _) = []
transpose xs      = (map head xs): (transpose $ map tail xs)



removeOne :: Eq a => [a] -> a -> [a]
removeOne (x: xs) e = if x == e then xs else x: removeOne xs e
removeOne []      _ = []



removeMany :: Eq a => [a] -> [a] -> [a]
removeMany = foldl removeOne



merge :: [[a]] -> [[b]] -> [[(a, b)]]
merge = zipWith zip



merge3 :: [[a]] -> [[b]] -> [[c]] -> [[(a, b, c)]]
merge3 = zipWith3 zip3



swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)



sample :: StdGen -> Int -> [a] -> [a]
sample g n xs = evalState (runRVar (R.sample n xs) StdRandom) g
