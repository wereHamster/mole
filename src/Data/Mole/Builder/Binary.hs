module Data.Mole.Builder.Binary where


import qualified Data.Set as S

import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint



binaryBuilder :: String -> String -> Handle -> AssetId -> IO Builder
binaryBuilder src contentType _ _ = do
    body <- BS.readFile src
    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result (PublicIdentifier $ fingerprint body $ T.pack src) $ Just (body, contentType)
        , sourceFingerprint = body
        }


-- | Like the 'binaryBuilder', but does not fingerprint the asset.
rawBuilder :: PublicIdentifier -> String -> String -> Handle -> AssetId -> IO Builder
rawBuilder pubId src contentType _ _ = do
    body <- BS.readFile src
    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result pubId (Just (body, contentType))
        , sourceFingerprint = body
        }
