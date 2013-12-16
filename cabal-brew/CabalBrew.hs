{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Options.Applicative
import           Prelude             hiding (FilePath)
import           Shelly


type ProjectName    = String
type ProjectVersion = String



main :: IO ()
main = execParser opts >>= print
    where opts' =   CB
                <$> strOption (metavar "NAME"    <> help "The name of the package to install.")
                <*> strOption (metavar "VERSION" <> help "The version to install.")
          opts  = info (helper <*> opts') (  fullDesc
                                          <> progDesc "This installs a Haskell program\
                                                      \ to be managed by Homebrew."
                                          <> header "cabal-brew - install Haskell\
                                                    \ packages programs.")

data CabalBrew = CB
               { projectName    :: ProjectName
               , projectVersion :: ProjectVersion
               } deriving (Show)


