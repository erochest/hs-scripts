{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad
import           Data.Monoid

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Options.Applicative
import           System.Random.Shuffle


data ShuffleUnit = Char
                 | Word
                 | Line
                 deriving (Eq, Show, Read)

data ShuffleOpts = ShuffleOpts
                 { shuffleUnit :: ShuffleUnit
                 } deriving (Show)


splitUnits :: ShuffleUnit -> T.Text -> [T.Text]
splitUnits Char = T.chunksOf 1
splitUnits Word = T.words
splitUnits Line = T.lines

joinUnits :: ShuffleUnit -> [T.Text] -> T.Text
joinUnits Char = T.concat
joinUnits Word = T.unwords
joinUnits Line = T.unlines

shuffleText :: ShuffleUnit -> T.Text -> IO T.Text
shuffleText unit = pure . joinUnits unit <=< shuffleM . splitUnits unit

main :: IO ()
main = do
    ShuffleOpts{..} <- execParser opts
    TIO.getContents >>= shuffleText shuffleUnit >>= TIO.putStr

    where opts' =   ShuffleOpts
                <$> option auto (  long "unit"
                                <> short 'u'
                                <> metavar "UNIT"
                                <> value Line
                                <> help "The unit to shuffle on. One of Char, \
                                        \Word, or Line. Default is 'Line'."
                                )
          opts  = info (helper <*> opts') (  fullDesc
                                          <> progDesc "Shuffle the input on unit."
                                          <> header "shuffle"
                                          )


