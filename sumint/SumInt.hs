{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List (foldl')
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

intSum :: Int -> Int -> Int
intSum = (+)

main :: IO ()
main = TIO.interact $ T.pack . show . foldl' intSum 0 . map (read . T.unpack) . T.lines


