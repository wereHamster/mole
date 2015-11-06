{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.Internal.Fingerprint
    ( contentHash
    , fingerprint
    ) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word
import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Crypto.Hash.SHA3

import           System.FilePath



contentHash :: ByteString -> Text
contentHash body = T.decodeUtf8 $ BS.map (\x -> alnum $ rem x 62) $ hash 512 body
  where
    alnum :: Word8 -> Word8
    alnum x
        | x < 26 = ((x     ) + 65)
        | x < 52 = ((x - 26) + 97)
        | x < 62 = ((x - 52) + 48)
        | otherwise = error $ "Out of range: " ++ show x


fingerprint :: ByteString -> String -> Text
fingerprint body name = mconcat
    [ T.take prefixLength h
    , "-"
    , T.pack (takeBaseName name)
    , "-"
    , T.drop prefixLength h
    , T.pack (takeExtension name)
    ]
  where
    h = contentHash body
    prefixLength = 9
