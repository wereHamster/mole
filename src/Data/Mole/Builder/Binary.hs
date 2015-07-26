module Data.Mole.Builder.Binary where


import qualified Data.Set as S

import qualified Data.ByteString as BS

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint



binaryBuilder :: String -> String -> Handle -> AssetId -> IO Builder
binaryBuilder src contentType _ _ = do
    body <- BS.readFile src
    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result (fingerprint body src) $ Just (body, contentType)
        }
