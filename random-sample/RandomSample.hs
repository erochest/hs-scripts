{-# LANGUAGE TupleSections #-}


module Main where


import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import           Data.Maybe
import           Data.Ord
import           System.Environment
import           System.Random


type LineCache a = M.HashMap Int a
data Triple a b c = T a b c
                  deriving (Show)


tfst :: Triple a b c -> a
tfst (T a _ _) = a

tsnd :: Triple a b c -> b
tsnd (T _ b _) = b

ttrd :: Triple a b c -> c
ttrd (T _ _ c) = c


pairs :: [a] -> [[a]]
pairs (x:y:zs) = [x, y] : pairs zs
pairs [x]      = [[x]]
pairs []       = []

inSample :: Double -> Triple Int [Double] a -> Bool
inSample n (T i (j:_) _) = j <= (n / fromIntegral i)

swapKeys :: LineCache a -> Int -> Int -> a -> LineCache a
swapKeys m k1 k2 v = M.insert k2 v $ M.delete k1 m

sample :: RandomGen g => g -> Int -> [a] -> [a]
sample g k xs = map snd
              . L.sortBy (comparing fst)
              . M.toList
              . L.foldl' sample' (M.fromList $ L.zipWith (,) ([0..] :: [Int]) xs1)
              . filter (inSample k')
              $ L.zipWith3 T [k..] (pairs $ randomRs (0.0 :: Double, 1.0) g) xs2
    where
        k'         = fromIntegral k
        (xs1, xs2) = L.splitAt k xs
        sample' m (T i (_:j:_) x) = swapKeys m rm i x
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
            B.interact (TE.encodeUtf8 . T.unlines . sample g k . T.lines . TE.decodeUtf8)
        Nothing -> putStrLn "usage: randomSample N"

