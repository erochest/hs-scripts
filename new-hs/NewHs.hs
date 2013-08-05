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
- * Set up a Vagrant environment for a dev- and build-box.
-
-}


module Main where

import qualified Data.Text as T
import           Shelly

default (T.Text)


main :: IO ()
main = shelly $ verbosely $ do
    undefined

