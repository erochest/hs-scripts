{-# LANGUAGE TupleSections #-}


module Main where


import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.HashMap.Strict           as M
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord
import           Data.Traversable
import           System.Environment
import           System.Random.Mersenne.Pure64


type LineCache a = M.HashMap Int a


randomPairs :: (PureMT, Int) -> a -> ((PureMT, Int), (Int, [Double], a))
randomPairs (g0, n) x =
    let (r0, g1) = randomDouble g0
        (r1, g2) = randomDouble g2
    in  ((g2, succ n), (n, [r0, r1], x))

inSample :: Double -> (Int, [Double], a) -> Bool
inSample n (i, j:_, _) = j <= (n / fromIntegral i)

swapKeys :: LineCache a -> Int -> Int -> a -> LineCache a
swapKeys m k1 k2 v = M.insert k2 v $ M.delete k1 m

sample :: PureMT -> Int -> [a] -> [a]
sample g k xs = map snd
              . L.sortBy (comparing fst)
              . M.toList
              . L.foldl' sample' (M.fromList $ zip ([0..] :: [Int]) xs1)
              . filter (inSample k')
              . snd
              $ mapAccumL randomPairs (g, 0 :: Int) xs2
    where
        k'         = fromIntegral k
        (xs1, xs2) = L.splitAt k xs
        sample' m (i, _:j:_, x) = swapKeys m rm i x
            where ks = M.keys m
                  rm = ks !! truncate (j * k')


readInt :: String -> Int
readInt = read


main :: IO ()
main = do
    args <- fmap readInt . listToMaybe <$> getArgs
    case args of
        Just k  -> do
            g <- newPureMT
            B.interact (B.unlines . sample g k . B.lines)
        Nothing -> putStrLn "usage: randomSample N"

