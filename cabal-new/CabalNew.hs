{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


-- mkdir ~/p/PROJECT
--
-- git init
--
-- cabal init ...
-- git add .
-- git commit -m "cabal init"
--
-- pushd ~/p/hs-project/ ; ./apply-project ~/p/PROJECT ; popd
-- git add .
-- git commit -m "apply hs project"
--
-- touch README.md
-- git add README.md
-- git commit -m "README.md"
--
-- sandbox-init --enable-tests
--
-- hub create
-- git push -u origin master
--
-- For the future:
--
-- * set up a vagrant environment for a build box
-- * configuration file to pass defaults to `cabal init` (email, etc).

module Main where


import           Control.Applicative
import Data.Maybe (catMaybes)

import           ClassyPrelude             hiding ((</>), (<>))
import qualified Data.Text                 as T
import           Filesystem                (getHomeDirectory)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Shelly

default (T.Text)


expandUserDir :: FilePath -> IO FilePath
expandUserDir filepath =
    case FS.encodeString filepath of
        ('~':'/':xs) -> (</> xs) <$> getHomeDirectory
        _            -> return filepath

configDir :: String -> Sh FilePath
configDir = liftIO . expandUserDir . fromString


git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

gitAddAllCommit :: T.Text -> Sh ()
gitAddAllCommit msg = git_ "add" ["."] >> git_ "commit" ["-m", msg]

withCommit :: T.Text -> Sh a -> Sh a
withCommit msg op = op <* gitAddAllCommit msg

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

ifSet :: Text -> String -> Maybe Text
ifSet _ ""    = Nothing
ifSet key val = Just $ "--" <> key <> " = " <> T.pack val

ifTrue :: Text -> Bool -> Maybe Text
ifTrue _ False  = Nothing
ifTrue key True = Just $ "--" <> key

cabalInit :: CabalNew -> Sh ()
cabalInit CabalNew{..} =
    cabal_ "init" $ catMaybes [ Just   "--non-interactive"
                              , ifSet  "license"       projectLicense
                              , ifSet  "email"         projectEmail
                              , ifSet  "synopsis"      projectSynopsis
                              , ifTrue "is-library"    projectLibrary
                              , ifTrue "is-executable" projectExecutable
                              ]


main :: IO ()
main = do
    config <- execParser opts
    shelly $ verbosely $ do
        rootDir  <- configDir $ projectRootDir  config
        patchDir <- configDir $ projectPatchDir config
        let name       = T.pack $ projectName config
            projectDir = rootDir </> name
            private    = privateProject config

        mkdir_p projectDir

        chdir projectDir $ do
            git_ "init" []
            withCommit "cabal init" $ cabalInit config

            withCommit "apply hs project" $
                chdir patchDir $
                    run_ (patchDir </> "apply-project") [toTextIgnore projectDir]

            withCommit "README.md" $ touchfile "README.md"
            cabal_ "sandbox" ["init"]
            run_ "sandbox-init" ["--enable-tests"]

            unless private $ run_ "hub" ["create"] >> git_ "push" ["-u", "origin", "master"]
        echo "done."

    where opts' =   CabalNew
                <$> strOption (  short 'r' <> long "root-dir"
                              <> value "~/p/"
                              <> help "The root directory for all projects.")
                <*> strOption (  short 'p' <> long "project-name"
                              <> help "The project name.")
                <*> strOption (  short 'a' <> long "apply-project"
                              <> value (FS.encodeString "~/p/hs-project/")
                              <> help "The directory containing the project to apply a patch on this project with.")
                <*> switch    (  short 'P' <> long "private"
                              <> help "Don't publish this repository to github.")
                <*> strOption (  short 'l' <> long "license" <> value "Apache-2.0"
                              <> help "The cabal option for the license. Defaults to Apache 2.0.")
                <*> strOption (  short 'e' <> long "email"
                              <> help "The cabal option for the email.")
                <*> strOption (  short 's' <> long "synopsis"
                              <> help "The cabal option for the synopsis.")
                <*> strOption (  short 'c' <> long "category"
                              <> help "The cabal option for the category.")
                <*> switch    (  long "is-library"
                              <> help "The cabal option for the library.")
                <*> switch    (  long "is-executable"
                              <> help "The cabal option for the executable.")
          opts  = info (helper <*> opts') (  fullDesc
                                          <> progDesc "Create a new Haskell project with cabal, git, sandbox-init, and everything else."
                                          <> header "cabal-new - a utility to initialize a new Haskell project."
                                          )


data CabalNew = CabalNew
              { projectRootDir    :: String
              , projectName       :: String
              , projectPatchDir   :: String
              , privateProject    :: Bool
              , projectLicense    :: String
              , projectEmail      :: String
              , projectSynopsis   :: String
              , projectCategory   :: String
              , projectLibrary    :: Bool
              , projectExecutable :: Bool
              } deriving (Show)

