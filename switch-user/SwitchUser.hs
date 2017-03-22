{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Error
import           Control.Monad            (join, mzero, when)
import           Data.Bifunctor
import qualified Data.HashMap.Lazy        as M
import           Data.Semigroup           ((<>))
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Yaml                hiding (Parser)
import           Options.Applicative
import           Options.Applicative.Text
import           System.Directory
import           System.FilePath
import           System.Process


type UserLabel = T.Text

data Config
  = Config
  { configFile   :: !FilePath
  , configReport :: !Bool
  , configLabel  :: !UserLabel
  } deriving (Show, Eq)

data IdentityInfo
  = Identity
  { idEmail     :: !String
  , idCopyright :: !String
  } deriving (Show, Eq)

instance FromJSON IdentityInfo where
  parseJSON (Object o) =
    Identity <$> o .: "email" <*> o .: "copyright"
  parseJSON _ = mzero

type IdentityIndex = M.HashMap T.Text IdentityInfo

configParser :: IO (Parser Config)
configParser = do
  home <- getHomeDirectory
  return
    $   Config
    <$> strOption (  short 'c' <> long "config" <> metavar "CONFIG_FILE"
                    <> value (home </> ".switch-user.yaml")
                    <> help "The location of the config file. Default\
                            \ is '~/.switch-user.yaml'")
    <*> switch (  short 'r' <> long "report"
               <> help "Write out the files changed afterward.")
    <*> textArgument (  metavar "LABEL" <> value "personal"
                     <> help "The label to switch to. This is one\
                             \ of the keys in the config YAML file. \
                             \ It defaults to 'personal'.")

readConfig :: FilePath -> Script Object
readConfig = readConfig'

readConfig' :: FromJSON a => FilePath -> Script a
readConfig' = ExceptT
             . fmap (first prettyPrintParseException)
             . decodeFileEither

readIdentities :: FilePath -> Script IdentityIndex
readIdentities = readConfig'

getEmail :: Object -> Script String
getEmail = lookupString "email"

getCopyright :: Object -> Script String
getCopyright = lookupString "copyright"

lookupString :: T.Text -> Object -> Script String
lookupString key =
  hoistEither
  . fmap T.unpack
  . join
  . fmap (note "Not a string." . toString)
  . note (T.unpack $ "Missing key: " <> key)
  . M.lookup key

switchGit :: String -> Script ()
switchGit email =
  scriptIO $ callProcess "git" ["config", "--global"
                               , "user.email", email
                               ]

switchStack :: String -> String -> Script ()
switchStack email copyright = do
  home      <- scriptIO getHomeDirectory
  let stackConfig = home </> ".stack/config.yaml"
  stack     <- readConfig stackConfig
  templates <- (toObject =<< M.lookup "templates" stack) ?? "Missing templates."
  params    <- (toObject =<< M.lookup "params" templates) ?? "Missing params."
  let params'    = M.insert "author-email" (String $ T.pack email)
                   $ M.insert "copyright" (String $ T.pack copyright) params
      templates' = M.insert "params" (Object params') templates
      stack'     = M.insert "templates" (Object templates') stack
  scriptIO $ encodeFile stackConfig stack'

findLabel :: UserLabel -> IdentityIndex -> Either String IdentityInfo
findLabel label = note (T.unpack $ "Missing label: " <> label)
                  . M.lookup label

toObject :: Value -> Maybe Object
toObject (Object o) = Just o
toObject _          = Nothing

toString :: Value -> Maybe T.Text
toString (String t) = Just t
toString _          = Nothing

main :: IO ()
main = runScript $ do
  cp <- scriptIO configParser
  let opts = info (cp <**> helper)
             (  fullDesc
               <> progDesc "Switch users in the git config and other \
                           \places."
               <> header "switch users")
  Config{..} <- scriptIO $ execParser opts

  config <- readIdentities configFile
  Identity{idEmail=email, idCopyright=copyright} <- hoistEither
    $ findLabel configLabel config

  switchGit   email
  switchStack email copyright

  when configReport $ scriptIO $ do
    home <- getHomeDirectory
    TIO.putStrLn =<< TIO.readFile (home </> ".gitconfig")
    TIO.putStrLn =<< TIO.readFile (home </> ".stack/config.yaml")

