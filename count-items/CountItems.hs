{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO


type Group = (T.Text, Int)

countGroup :: [T.Text] -> Group
countGroup [x]    = (x, 1)
countGroup (x:xs) = (x, 1 + length xs)

showGroup :: Group -> T.Text
showGroup (x, i) = x <> "\t" <> T.pack (show i)

main :: IO ()
main =
    TIO.interact $ T.unlines . map (showGroup . countGroup) . L.group . T.lines

