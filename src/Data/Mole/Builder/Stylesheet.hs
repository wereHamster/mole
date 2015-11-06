module Data.Mole.Builder.Stylesheet where


import           Data.Traversable

import           Data.Map (Map)

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

import           Data.Maybe

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint

import           Data.CSS.Syntax.Tokens

import           Network.URI



tokenToAssetId :: Token -> Maybe AssetId
tokenToAssetId (Url x) = Just $ urlAssetId x
tokenToAssetId _       = Nothing


urlAssetId :: Text -> AssetId
urlAssetId x = AssetId $ case parseRelativeReference (T.unpack x) of
    Nothing  -> x
    Just uri -> T.pack $ uriPath uri


reconstructUrl :: Text -> Text -> Text
reconstructUrl x pubId = case parseRelativeReference (T.unpack x) of
    Nothing -> pubId
    Just uri -> T.pack $ show $ uri { uriPath = T.unpack pubId }


stylesheetBuilder :: String -> Handle -> AssetId -> IO Builder
stylesheetBuilder src _ _ = do
    body <- T.readFile src

    let Right tokens = tokenize body

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList (catMaybes $ map tokenToAssetId tokens)
        , packageAsset      = render tokens
        , sourceFingerprint = T.encodeUtf8 body
        }

  where
    render :: [Token] -> Map AssetId PublicIdentifier -> Either Error Result
    render tokens m = do
        newTokens <- for tokens $ \t -> case t of
            (Url x) -> case M.lookup (urlAssetId x) m of
                Nothing -> Left (UndeclaredDependency (AssetId x))
                Just (PublicIdentifier v) -> Right (Url $ reconstructUrl x v)
            _ -> Right t

        let body = T.encodeUtf8 $ serialize newTokens

        return $ Result (PublicIdentifier $ fingerprint body src) $ Just (body, "text/css")
