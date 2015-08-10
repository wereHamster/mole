module Data.Mole.Builder.Stylesheet where


import           Control.Monad

import           Data.Map (Map)

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.ByteString.Char8 (pack)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Data.Maybe

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint

import           Data.CSS.Syntax.Tokens



stylesheetBuilder :: String -> String -> Handle -> AssetId -> IO Builder
stylesheetBuilder pubId src _ _ = do
    body <- T.readFile src

    let Right tokens = tokenize body
    let deps = catMaybes $ (flip map) tokens $ \t ->
            case t of
                (Url x) -> Just $ AssetId (T.unpack x)
                _       -> Nothing

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r tokens
        }

  where
    r :: [Token] -> Map AssetId String -> Either Error Result
    r tokens m = do
        newTokens <- forM tokens $ \t -> case t of
            (Url x) -> case M.lookup (AssetId (T.unpack x)) m of
                Nothing -> Left (UndeclaredDependency (AssetId (T.unpack x)))
                Just v -> Right (Url $ T.pack v)
            _ -> return t

        let body = T.unpack $ serialize newTokens

        return $ Result (fingerprint (pack body) pubId) $ Just (pack body, "text/css")
