module Data.Mole.Builder.Stylesheet where


import           Control.Monad

import           Data.Map (Map)

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.ByteString.Char8 (pack)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T

import           Data.Maybe

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint

import           Data.CSS.Syntax.Tokens

import           Network.URI



urlAssetId :: String -> AssetId
urlAssetId x = case parseRelativeReference x of
    Nothing -> AssetId x
    Just uri -> AssetId $ uriPath uri

reconstructUrl :: String -> String -> String
reconstructUrl x pubId = case parseRelativeReference x of
    Nothing -> pubId
    Just uri -> show $ uri { uriPath = pubId }


stylesheetBuilder :: String -> String -> Handle -> AssetId -> IO Builder
stylesheetBuilder pubId src _ _ = do
    body <- T.readFile src

    let Right tokens = tokenize body
    let deps = catMaybes $ (flip map) tokens $ \t ->
            case t of
                (Url x) -> Just $ urlAssetId (T.unpack x)
                _       -> Nothing

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r tokens
        , sourceFingerprint = T.encodeUtf8 body
        }

  where
    r :: [Token] -> Map AssetId String -> Either Error Result
    r tokens m = do
        newTokens <- forM tokens $ \t -> case t of
            (Url x) -> case M.lookup (urlAssetId (T.unpack x)) m of
                Nothing -> Left (UndeclaredDependency (AssetId (T.unpack x)))
                Just v -> Right (Url $ T.pack $ reconstructUrl (T.unpack x) v)
            _ -> return t

        let bodyT = serialize newTokens
        let body = T.unpack bodyT

        return $ Result (fingerprint (pack body) pubId) $ Just (T.encodeUtf8 bodyT, "text/css")
