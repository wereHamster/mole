{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.JavaScript where


import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map (Map)

import           Data.Maybe
import           Data.Monoid

import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Encoding as T

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint
import           Data.Mole.Builder.Internal.Template



javascriptBuilder :: String -> String -> Handle -> AssetId -> IO Builder
javascriptBuilder pubId src _ _ = do
    body <- T.readFile src

    let t@(Template fragments) = template [("__assetUrl(\"", "\")")] body
    let deps = catMaybes $ (flip map) fragments $ \f -> case f of
                    (Lit _) -> Nothing
                    (Var _ x) -> Just $ AssetId x

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r t
        , sourceFingerprint = T.encodeUtf8 body
        }

  where
    r :: Template -> Map AssetId String -> Either Error Result
    r t m = do
        body <- render t $ \(a,b) x -> do
              case M.lookup (AssetId x) m of
                   Nothing -> Left (UndeclaredDependency (AssetId x))
                   Just v -> Right $ a <> T.pack v <> b

        let body' = T.encodeUtf8 body
        return $ Result (fingerprint body' pubId) $ Just (body', "text/javascript")
