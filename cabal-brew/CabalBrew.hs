{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           Shelly


type ProjectName    = Text
type ProjectVersion = Text

data CabalBrew = CB
               { packageName    :: ProjectName
               , packageVersion :: ProjectVersion
               } deriving (Show)


brew_ :: Text -> [Text] -> Sh ()
brew_ = command1_ "brew" []

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabalBrew :: CabalBrew -> Sh ()
cabalBrew CB{..} = do
    whenM (test_d sandbox) $ do
        echo $ "Cleaning out old keg for " <> keg
        brew_ "unlink" [keg]
        rm_rf sandbox
    chdir "/tmp" $ do
        whenM (test_f "cabal.sandbox.config") $ rm "cabal.sandbox.config"
        echo $ "cabal " <> packageName <> "-" <> packageVersion <> " => " <> toTextIgnore sandbox
        cabal_ "sandbox" ["init", "--sandbox=" <> toTextIgnore sandbox]
        cabal_ "install" [packageName <> "-" <> packageVersion]
        brew_ "link" ["--overwrite", keg]
    where keg     = "cabal-" <> packageName
          sandbox = FS.concat [ "/usr"
                              , "local"
                              , "Cellar"
                              , fromText keg
                              , fromText packageVersion
                              ]

main :: IO ()
main = execParser opts >>= shelly . verbosely . cabalBrew
    where opts' =   CB
                <$> textArg (metavar "NAME"    <> help "The name of the package to install.")
                <*> textArg (metavar "VERSION" <> help "The version to install.")
          opts  = info (helper <*> opts') (  fullDesc
                                          <> progDesc "This installs a Haskell program\
                                                      \ to be managed by Homebrew."
                                          <> header "cabal-brew - install Haskell\
                                                    \ packages programs.")

textArg :: Mod ArgumentFields Text -> Parser Text
textArg = argument (Just . T.pack)

textOption :: Mod OptionFields Text -> Parser Text
textOption fields = nullOption (reader (pure . T.pack) <> fields)


