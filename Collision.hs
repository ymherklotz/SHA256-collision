#!/usr/bin/env stack
-- stack --resolver lts-13.7 --install-ghc runghc --package cryptonite --package text --package bytestring --package memory
{-# LANGUAGE OverloadedStrings #-}
import           Crypto.Hash             (Digest, hash)
import           Crypto.Hash.Algorithms  (SHA256)
import qualified Data.ByteArray          as M
import qualified Data.ByteString         as B
import           Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy    as L
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T
import           Prelude                 hiding (repeat)

charSet :: String
charSet = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z']

start :: String
start = "CO409CryptographyEngineeringRunsNowForItsFifthYear"

val :: [String]
val = [ [x, y , z, a, b, c] | x <- charSet
                            , y <- charSet
                            , z <- charSet
                            , a <- charSet
                            , b <- charSet
                            , c <- charSet ]

showBS :: B.ByteString -> Text
showBS = T.decodeUtf8
       . L.toStrict
       . toLazyByteString
       . byteStringHex

hash256 :: (M.ByteArrayAccess ba) => ba -> Digest SHA256
hash256 = hash

hHash :: B.ByteString -> Digest SHA256
hHash = hash256 . hash256

repeat :: [String] -> IO ()
repeat [] = putStrLn "No match found"
repeat (l:ls) = do
  let v = showBS . M.convert . hHash . T.encodeUtf8 $ T.pack (start <> l)
  case T.take 6 v of
    "000000" -> do
      putStr (start <> l <> ": ")
      T.putStrLn v
    _ -> repeat ls

-- to check: echo -n 'CO409CryptographyEngineeringRunsNowForItsFifthYear00zQVx' | \
-- openssl dgst -binary -sha256 | openssl dgst -sha256
main :: IO ()
main = repeat val
