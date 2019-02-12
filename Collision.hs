#!/usr/bin/env stack
-- stack --resolver lts-13.7 --install-ghc runghc --package cryptonite --package text --package bytestring --package memory
{-# LANGUAGE OverloadedStrings #-}
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Hash (hash, Digest)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import qualified Data.ByteArray as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (toLazyByteString, byteStringHex)

charSet :: Text
charSet = T.pack $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

showBS :: (B.ByteArrayAccess ba) => ba -> Text
showBS = M.convert
       . T.decodeUtf8
       . L.toStrict
       . toLazyByteString
       . byteStringHex

hash256 :: (M.ByteArrayAccess ba) => ba -> Digest SHA256
hash256 = hash

hHash :: B.ByteString -> Digest SHA256
hHash = hash256 . hash256

main :: IO ()
main = do
  let val = "CO409CryptographyEngineeringRunsNowForItsFifthYear" :: Text
  T.putStrLn . showBS . hHash $ T.encodeUtf8 val
