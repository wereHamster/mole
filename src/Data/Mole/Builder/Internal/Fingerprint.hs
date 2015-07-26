module Data.Mole.Builder.Internal.Fingerprint
    ( contentHash
    , fingerprint
    ) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (unpack)
import           Data.Word

import           Crypto.Hash.SHA3

import           System.FilePath



contentHash :: ByteString -> String
contentHash body =  unpack $ BS.map (\x -> alnum $ rem x 62) $ hash 512 body
  where
    alnum :: Word8 -> Word8
    alnum x
        | x < 26 = ((x     ) + 65)
        | x < 52 = ((x - 26) + 97)
        | x < 62 = ((x - 52) + 48)
        | otherwise = error $ "Out of range: " ++ show x


fingerprint :: ByteString -> String -> String
fingerprint body name = mconcat
    [ take prefixLength h
    , "-"
    , takeBaseName name
    , "-"
    , drop prefixLength h
    , takeExtension name
    ]
  where
    h = contentHash body
    prefixLength = 9
