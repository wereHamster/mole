module Data.Mole.Builder.External where


import qualified Data.Set as S

import           Data.Mole.Types
import           Data.ByteString.Char8 (pack)



externalBuilder :: PublicIdentifier -> Handle -> AssetId -> IO Builder
externalBuilder pubId _ _ = do
    return $ Builder
        { assetSources      = S.empty
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result pubId Nothing
        , sourceFingerprint = pack pubId
        }
