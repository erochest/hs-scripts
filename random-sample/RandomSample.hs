{-# LANGUAGE TupleSections #-}


module Main where


import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict        as M
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Ord
import           System.Environment
import           System.Random


type LineCache a = M.HashMap Int a


triple :: a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)

pairs :: [a] -> [[a]]
pairs (x:y:zs) = [x, y] : pairs zs
pairs [x]      = [[x]]
pairs []       = []

inSample :: Double -> (Int, [Double], a) -> Bool
inSample n (i, (j:_), _) = j <= (n / fromIntegral i)

swapKeys :: LineCache a -> Int -> Int -> a -> LineCache a
swapKeys m k1 k2 v = M.insert k2 v $ M.delete k1 m

sample :: RandomGen g => g -> Int -> [a] -> [a]
sample g k xs = map snd
              . L.sortBy (comparing fst)
              . M.toList
              . L.foldl' sample' (M.fromList $ L.zipWith (,) ([0..] :: [Int]) xs1)
              . filter (inSample k')
              $ L.zipWith3 triple [k..] (pairs $ randomRs (0.0 :: Double, 1.0) g) xs2
    where
        k'         = fromIntegral k
        (xs1, xs2) = L.splitAt k xs
        sample' m (i, (_:j:_), x) = swapKeys m rm i x
            where ks = M.keys m
                  rm = ks !! truncate (j * k')


readInt :: String -> Int
readInt = read


main :: IO ()
main = do
    args <- fmap readInt . listToMaybe <$> getArgs
    case args of
        Just k  -> do
            g <- getStdGen
            B.interact (B.unlines . sample g k . B.lines)
        Nothing -> putStrLn "usage: randomSample N"

