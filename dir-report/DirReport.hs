{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Format
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO         as TIO
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Shelly


type Index = M.HashMap (Maybe T.Text) FileSum
type FileSum = (Sum Int, Sum Integer)

index :: Index -> FilePath -> IO Index
index idx fp = do
    sz <- getSize fp
    return $ M.insertWith mappend (extension fp) (Sum 1, Sum sz) idx

indexSh :: Index -> FilePath -> Sh Index
indexSh idx fp = liftIO $ index idx fp

render :: Index -> TL.Text
render = toLazyText . mconcat . map (uncurry renderFileSum) . L.sort . M.toList

renderFileSum :: Maybe T.Text -> FileSum -> Builder
renderFileSum ext (Sum count, Sum sz) =
    build "{} => {} / {}\n"
          (right 10 ' ' (fromMaybe "" ext), left 8 ' ' count, left 8 ' ' (size sz))

status :: T.Text -> FilePath -> Sh FilePath
status msg fp = echo (msg <> toTextIgnore fp) >> return fp

size :: Integer -> Builder
size num = go (fromIntegral num) [" B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB"]
    where
        go :: Double -> [Builder] -> Builder
        go n (s:ss) | abs n < 1024.0 = build "{}{}" (fixed 1 n, s)
                    | otherwise      = go (n / 1024.0) ss
        go n []                      = build "{}YB" . Only $ fixed 1 n


main :: IO ()
main = shelly $ verbosely $   pwd
                          >>= status "Generating report for "
                          >>= findFold indexSh M.empty
                          >>= liftIO . TIO.putStrLn . render
