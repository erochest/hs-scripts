{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           ClassyPrelude
import qualified Data.Text.Lazy as LT
import           Shelly

default (LT.Text)


main :: IO ()
main = putStrLn "dedup"


