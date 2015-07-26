module Data.Mole.Builder.Stylesheet where

import           Data.Map (Map)

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.ByteString.Char8 (pack)

import qualified Data.Text as T

import           Data.Maybe

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint
import           Data.Mole.Builder.Internal.Template



stylesheetBuilder :: String -> String -> Handle -> AssetId -> IO Builder
stylesheetBuilder pubId src _ _ = do
    body <- readFile src

    let t@(Template fragments) = template body
    let deps = catMaybes $ (flip map) fragments $ \f ->
            case f of
                (Lit _) -> Nothing
                (Var x) -> Just $ AssetId (T.unpack x)

    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList deps
        , packageAsset      = r t
        }

  where
    r :: Template -> Map AssetId String -> Either Error Result
    r t m = do
        body <- render t $ \x -> do
            case M.lookup (AssetId (T.unpack x)) m of
                Nothing -> Left (UndeclaredDependency (AssetId (T.unpack x)))
                Just v -> Right $ T.pack v

        return $ Result (fingerprint (pack body) pubId) $ Just (pack body, "text/css")
