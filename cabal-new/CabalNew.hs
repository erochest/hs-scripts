{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


-- TODO: Flag to use castle (and maybe specify the castle name). Otherwise,
-- it uses a castle with the same name as the project.

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
import qualified Data.Char                 as C

import           ClassyPrelude             hiding ((</>), (<>))
import qualified Data.Text                 as T
import           Filesystem                (getHomeDirectory)
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative
import           Shelly

default (T.Text)


execStub :: Text
execStub = "\
    \{-# LANGUAGE OverloadedStrings #-}\n\n\
    \module Main where\n\n\
    \main :: IO ()\n\
    \main = undefined\n\n"


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
ifSet key val = Just $ "--" <> key <> "=" <> T.pack val

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

sed :: FilePath -> (T.Text -> T.Text) -> Sh ()
sed fp f = withTmpDir $ \tmpDir -> do
    let tmpFile = tmpDir FS.</> FS.filename fp
    mv fp tmpFile
    writefile fp . T.unlines . map f . T.lines =<< readfile tmpFile

setMainIs :: FilePath -> String -> Sh ()
setMainIs cabalPath mainFile = sed cabalPath $ \line ->
    if "-- main-is:" `T.isInfixOf` line
        then takeWhile C.isSpace line <> "main-is:             " <> T.pack mainFile
        else line

toTitleCase :: Bool -> String -> String
toTitleCase _     []       = []
toTitleCase True  (c:cs)   = C.toUpper c : toTitleCase False cs
toTitleCase _     ('-':cs) = toTitleCase True cs
toTitleCase False (c:cs)   = c : toTitleCase False cs


main :: IO ()
main = do
    config <- execParser opts
    shelly $ verbosely $ do
        rootDir  <- configDir $ projectRootDir  config
        patchDir <- configDir $ projectPatchDir config
        let name       = T.pack $ projectName config
            projectDir = rootDir </> name
            private    = privateProject config
            mainFile   = toTitleCase True (projectName config) ++ ".hs"
            mainPath   = FS.decodeString mainFile
            cabal      = FS.decodeString (projectName config) FS.<.> "cabal"

        mkdir_p projectDir

        chdir projectDir $ do
            git_ "init" []
            withCommit "cabal init" $ cabalInit config

            withCommit "apply hs project" $
                chdir patchDir $
                    run_ (patchDir </> "apply-project") [toTextIgnore projectDir]

            withCommit "README.md" $ touchfile "README.md"

            when (projectExecutable config) $
                withCommit "Added stub main file." $ do
                    writefile mainPath execStub
                    setMainIs cabal mainFile

            cabal_ "sandbox" ["init"]
            run_ "sandbox-init" ["--enable-tests"]

            unless private $ run_ "hub" ["create"] >> git_ "push" ["-u", "origin", "master"]
        echo "done."

    where opts' =   CabalNew
                <$> strOption (  short 'r' <> long "root-dir"
                              <> value "~/p/"
                              <> help "The root directory for all projects\
                                      \ (default '~/p/').")
                <*> strOption (  short 'p' <> long "project-name"
                              <> help "The project name.")
                <*> strOption (  short 'a' <> long "apply-project"
                              <> value (FS.encodeString "~/p/hs-project/")
                              <> help "The directory containing the project\
                                      \ to apply a patch on this project\
                                      \ with (default '~/p/hs-project/').")
                <*> switch    (  short 'P' <> long "private"
                              <> help "Don't publish this repository to github.")
                <*> strOption (  short 'l' <> long "license" <> value "Apache-2.0"
                              <> help "The cabal option for the license.\
                                      \ (defaults 'Apache-2.0').")
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

