module Data.Mole.Builder.Html
    ( htmlBuilder
    ) where


import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.ByteString.Char8 as BS

import           Data.Maybe

import           Text.HTML.TagSoup

import           Data.Mole.Types



htmlTags :: [(String,String)]
htmlTags =
    [ ("link", "href")
    , ("img", "src")
    , ("script", "src")
    , ("a", "href")
    , ("meta", "content")
    ]

htmlBuilder :: String -> String -> Handle -> AssetId -> IO Builder
htmlBuilder pubId src _ _ = do
    body <- readFile src
    let tags = parseTags body
    let deps = catMaybes $ map toDep tags
    return $ Builder (S.singleton src) (S.fromList deps) (render tags)

  where
    toDep :: Tag String -> Maybe AssetId
    toDep (TagOpen tag attrs) = do
        attr <- lookup tag htmlTags
        AssetId <$> lookup attr attrs
    toDep _ = Nothing

    render :: [Tag String] -> Map AssetId String -> Either Error Result
    render tags m = do
        t' <- forM tags $ \t -> do
            insertResult m t

        let body = BS.pack $ renderTags t'
        return $ Result pubId $ Just (body, "text/html")

    insertResult :: Map AssetId String -> Tag String -> Either Error (Tag String)
    insertResult m t@(TagOpen tag attrs) = case lookup tag htmlTags of
        Nothing -> pure t
        Just attr -> TagOpen <$> (pure tag) <*> (mapM (overrideAttr attr m) attrs)
    insertResult _ t = pure t

    overrideAttr :: String -> Map AssetId String -> (String,String) -> Either Error (String,String)
    overrideAttr n m (k,v)
        | k == n    = case M.lookup (AssetId v) m of
            Just v' -> Right (k,v')
            Nothing -> case M.lookup (AssetId $ tail v) m of
                Just v'' -> Right (k,v'')
                Nothing -> Left (UndeclaredDependency (AssetId v))
        | otherwise = Right (k,v)
