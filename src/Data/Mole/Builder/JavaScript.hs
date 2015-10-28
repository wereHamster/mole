{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.JavaScript where


import           Control.Monad

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map (Map)

import           Data.Maybe
import           Data.Monoid

import qualified Data.Text          as T
import           Data.ByteString.Char8 (pack)

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint
import           Data.Mole.Builder.Internal.Template



javascriptBuilder :: String -> String -> Handle -> AssetId -> IO Builder
javascriptBuilder pubId src _ _ = do
    body <- readFile src

    let t@(Template fragments) = template [("__assetUrl(\"", "\")")] body
    let deps = catMaybes $ (flip map) fragments $ \f -> case f of
                    (Lit _) -> Nothing
                    (Var _ x) -> Just $ AssetId (T.unpack x)

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r t
        }

  where
    r :: Template -> Map AssetId String -> Either Error Result
    r t m = do
        body <- render t $ \(a,b) x -> do
              case M.lookup (AssetId (T.unpack x)) m of
                   Nothing -> Left (UndeclaredDependency (AssetId (T.unpack x)))
                   Just v -> Right $ a <> T.pack v <> b

        return $ Result (fingerprint (pack body) pubId) $ Just (pack body, "text/javascript")
