{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE BangPatterns               #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           ClassyPrelude
import           Control.Monad (filterM, foldM)
import qualified Crypto.Hash               as H
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.HashMap.Strict       as M
import qualified Data.Text.Lazy            as LT
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly


default (LT.Text)


-- | This is the type for SHA1 hax digests.
type Sha1String = BS.ByteString

-- | The type for the index of hashes to files.
type HashIndex  = M.HashMap Sha1String [FS.FilePath]

-- | This gets the SHA1 hash for a file.
hashFile :: FS.FilePath -> IO (H.Digest H.SHA1)
hashFile = fmap H.hashlazy . BSL.readFile . FS.encodeString

-- | This inserts a file into the index at a SHA1 hash point.
indexHash :: H.Digest H.SHA1 -> FS.FilePath -> HashIndex -> HashIndex
indexHash sha1 filePath =
    M.insertWith (++) (H.digestToHexByteString sha1) [filePath]

-- | This hashes and indexes a file by its SHA1.
hashIndexFile :: MonadIO m => HashIndex -> FS.FilePath -> m HashIndex
hashIndexFile index filePath = do
    !sha1 <- liftIO $ hashFile filePath
    return $ indexHash sha1 filePath index

-- | This prints the hash and files.
printIndex :: Sha1String -> [FS.FilePath] -> IO ()
printIndex sha1 = mapM_ writePath
    where writePath p = BS.putStr sha1 >> putStrLn ("\t" <> toTextIgnore p)


main :: IO ()
main = shelly $ verbosely $ do
    index <- foldM hashIndexFile M.empty =<< filterM test_f =<< Shelly.find "."
    liftIO . mapM_ (uncurry printIndex) . filter ((> 1) . length . snd) $ M.toList index


