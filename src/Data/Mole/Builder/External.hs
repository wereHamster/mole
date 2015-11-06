module Data.Mole.Builder.External where


import qualified Data.Set as S
import qualified Data.Text.Encoding as T

import           Data.Mole.Types



externalBuilder :: PublicIdentifier -> Handle -> AssetId -> IO Builder
externalBuilder pubId _ _ = do
    return $ Builder
        { assetSources      = S.empty
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result pubId Nothing
        , sourceFingerprint = T.encodeUtf8 $ unPublicIdentifier pubId
        }
