{-# LANGUAGE DeriveDataTypeable #-}


module Main where


import           Control.Applicative
import           Control.Error
import           Data.Attoparsec.Text
import           Data.Data
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import           System.Console.CmdArgs

-- Command-line processing

data SpanArg = Span
             { spanArg :: String
             } deriving (Show, Data, Typeable)

type Span = (Maybe Int, Maybe Int)

spanSpec :: SpanArg
spanSpec = Span { spanArg = def &= args &= typ "M-N"
                } &= summary "Slice STDIN."
                  &= details ["M is the first line, and N is the \
                              \last.  Either may be ommitted, \
                              \meaning from the first line or to \
                              \the last line, respectively."]

parseSpan :: SpanArg -> Either String Span
parseSpan = parseOnly spanPair . T.pack . spanArg
    where spanPair =   (,)
                   <$> optional decimal
                   <*  char '-'
                   <*> optional decimal

-- sanity check

checkSpan :: Span -> Either String Span
checkSpan s@(Just f, Just t) | f <= t    = Right s
                             | otherwise = Left "M must be greater than N."
checkSpan s                              = Right s

-- slice function

slice :: Span -> [TL.Text] -> [TL.Text]
slice (f, t) =
        maybe id (L.take . flip (-) (fromMaybe 0 f)) t . maybe id L.drop f

{- A case analysis of the monstrosity above:
 - - (Nothing, Nothing) = id               . id
 - - (Just f', Nothing) = L.drop f'        . id
 - - (Nothing, Just t') = L.take (t' - 0)  . id
 - - (Just f', Just t') = L.take (t' - f') . L.drop f' 
 -}

{- Originally:
 - slice (Nothing, Nothing) = id
 - slice (Just f,  Nothing) = L.drop f
 - slice (Nothing, Just t)  = L.take t
 - slice (Just f,  Just t)  = L.take l . L.drop f
 -     where l = t - f
 -}

-- Main

main :: IO ()
main = runScript $ do
    spanPair <- scriptIO (cmdArgs spanSpec) >>=
                hoistEither . parseSpan     >>=
                hoistEither . checkSpan
    scriptIO . TIO.interact $ TL.unlines . slice spanPair . TL.lines

