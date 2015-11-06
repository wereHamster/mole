module Data.Mole.Builder.Stylesheet where


import           Control.Monad

import           Data.Map (Map)

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.ByteString.Char8 (pack)

import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

import           Data.Maybe

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint

import           Data.CSS.Syntax.Tokens

import           Network.URI



urlAssetId :: Text -> AssetId
urlAssetId x = case parseRelativeReference (T.unpack x) of
    Nothing -> AssetId x
    Just uri -> AssetId $ T.pack $ uriPath uri

reconstructUrl :: Text -> Text -> Text
reconstructUrl x pubId = case parseRelativeReference (T.unpack x) of
    Nothing -> pubId
    Just uri -> T.pack $ show $ uri { uriPath = T.unpack pubId }


stylesheetBuilder :: String -> Handle -> AssetId -> IO Builder
stylesheetBuilder src _ _ = do
    body <- T.readFile src

    let Right tokens = tokenize body
    let deps = catMaybes $ (flip map) tokens $ \t ->
            case t of
                (Url x) -> Just $ urlAssetId x
                _       -> Nothing

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r tokens
        , sourceFingerprint = T.encodeUtf8 body
        }

  where
    r :: [Token] -> Map AssetId PublicIdentifier -> Either Error Result
    r tokens m = do
        newTokens <- forM tokens $ \t -> case t of
            (Url x) -> case M.lookup (urlAssetId x) m of
                Nothing -> Left (UndeclaredDependency (AssetId x))
                Just (PublicIdentifier v) -> Right (Url $ reconstructUrl x v)
            _ -> return t

        let bodyT = serialize newTokens
        let body = T.unpack bodyT

        return $ Result (PublicIdentifier $ fingerprint (pack body) src) $ Just (T.encodeUtf8 bodyT, "text/css")
