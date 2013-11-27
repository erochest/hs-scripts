{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Data.Maybe (catMaybes)

import           ClassyPrelude hiding ((<>))
import qualified Data.Text as T
import           Options.Applicative
import           Shelly

default (T.Text)


testPlugins :: [T.Text]
testPlugins = [ "tasty"
              , "tasty-golden"
              , "tasty-quickcheck"
              , "tasty-hspec"
              , "hspec"
              , "QuickCheck"
              ]

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabalSandbox_ :: T.Text -> [T.Text] -> Sh ()
cabalSandbox_ cmd args = command1_ "cabal" [] "sandbox" $ cmd : args

boolMaybe :: Bool -> a -> Maybe a
boolMaybe True  x = Just x
boolMaybe False _ = Nothing


main :: IO ()
main = do
    ci@SandboxInit{..} <- execParser opts
    shelly $ verbosely $ do

        let installArgs = catMaybes [ boolMaybe enableTests     "--enable-tests"
                                    , boolMaybe enableProfiling "--enable-library-profiling"
                                    , boolMaybe enableCoverage  "--enable-library-coverage"
                                    ]
        when deleteSandbox $ do
            cabalSandbox_ "delete" [] >> cabalSandbox_ "init" []
            dist_exists <- test_d "dist"
            when dist_exists $ rm_rf "dist"
        cabal_ "install"   ["--only-dependencies"]
        cabal_ "install"   testPlugins
        cabal_ "configure" installArgs

    where opts' =   SandboxInit
                <$> switch (short 't' <> long "enable-tests"     <> help "Enable tests in tests.")
                <*> switch (short 'p' <> long "enable-profiling" <> help "Enable profiling in configuration.")
                <*> switch (short 'c' <> long "enable-coverage"  <> help "Enable coverage in configuration.")
                <*> switch (short 'd' <> long "delete"           <> help "Delete the sandbox before building.")
          opts  = info (helper <*> opts')
                       (  fullDesc
                       <> progDesc "Initialize the cabal sandbox environment."
                       <> header "sandbox-init - a utility to initialize a sandbox for a cabal project."
                       )


data SandboxInit = SandboxInit
                 { enableTests     :: Bool
                 , enableProfiling :: Bool
                 , enableCoverage  :: Bool
                 , deleteSandbox   :: Bool
                 } deriving (Show)

