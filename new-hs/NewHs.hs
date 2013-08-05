{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-| This program initializes a new Haskell project. Currently, it does these
- steps.
-
- * Create a new directory with the name of the project.
-
- * Run `git init` in the project.
-
- * Run `cabal init` with the name of the package and some other defaults
-   (but prompting for other things).
-
- * `git add --all`
-
- * `git commit -m "cabal init"`
-
- * `echo "# PROJECT" > README.md`
-
- * `git add README.md`
-
- * `git commit -m "README"`
-
- * `hub create`
-
- * `git push -u origin master`
-
- Possible improvements:
-
- * Add specs boilerplate for the project.
-
- * Install dependencies with `cabal-dev`.
-
- * Set up a Vagrant environment for a dev- and build-box.
-
- * More options for passing to `cabal init`.
-
- * Configuration file for defaults (default directory, email, license, etc.).
-
- * Option for private (i.e., not on Github) projects.
-
-}


module Main where


import           Control.Applicative
import           Data.Monoid
import           System.Environment

import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly

default (T.Text)


-- Configuration
projectRootDirectory :: Sh FS.FilePath
projectRootDirectory = maybe "./p" ((FS.</> "p") . fromText) <$> get_env "HOME"

defaultCabalOpts :: [T.Text]
defaultCabalOpts = [ "--non-interactive"
                   , "--version=0.0.1"
                   , "--author='Eric Rochester'"
                   , "--email=erochest@gmail.com"
                   , "--license=Apache-2.0"
                   , "--is-executable"
                   , "--is-library"
                   -- , "--homepage=https://github.com/erochest/PROJECT"
                   ]

git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

hub_ :: T.Text -> [T.Text] -> Sh ()
hub_ = command1_ "hub" []

newProject :: T.Text -> Sh ()
newProject projectName = do
    projectDir <- (FS.</> fromText projectName) <$> projectRootDirectory
    echo $ "Creating project directory: " <> toTextIgnore projectDir
    mkdir_p projectDir
    cd projectDir
    git_ "init" []
    let homepage = "--homepage=https://github.com/erochest/" <> projectName
        package  = "--package-name=" <> projectName
    cabal_ "init" (package:homepage:defaultCabalOpts)
    git_ "add" ["--all"]
    git_ "commit" ["-m", "cabal init"]
    writefile "README.md" $ "\n# " <> projectName <> "\n\n"
    git_ "add" ["README.md"]
    git_ "commit" ["-m", "README"]
    hub_ "create" []
    git_ "push" ["-u", "origin", "master"]
    echo "done"

main :: IO ()
main = shelly $ verbosely $ do
    args <- liftIO getArgs
    case args of
        []              -> error "You must specify a project name."
        (projectName:_) -> newProject (T.pack projectName)

