module Data.Mole.Builder.External where


import qualified Data.Set as S
import qualified Data.Text.Encoding as T

import           Data.Mole.Types



-- | The external builder is used to replace 'AssetId's with a constant.
--
-- One use case is if you want to insert a version number or git revision
-- in a HTML or JavaScript file:
--
--    <meta name="version" content="VERSION" />
--
-- and then add @--asset VERSION=`git rev-parse HEAD`@ to the command line.

externalBuilder :: PublicIdentifier -> Handle -> AssetId -> IO Builder
externalBuilder pubId _ _ = do
    return $ Builder
        { assetSources      = S.empty
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result pubId Nothing
        , sourceFingerprint = T.encodeUtf8 $ unPublicIdentifier pubId
        }
