module Data.Mole.Builder.Html
    ( htmlBuilder
    ) where


import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.ByteString.Char8 as BS

import qualified Data.Text as T

import           Data.Maybe
import           Data.CSS.Syntax.Tokens

import           Text.HTML.TagSoup

import           Data.Mole.Types
import           Data.Mole.Builder.Stylesheet



data Transformer = Transformer
    { tagSelector       :: String -> Bool
    , attributeName     :: String
    , depExtractor      :: String -> [AssetId]
    , attributeRenderer :: Map AssetId String -> String -> Either Error String
    }

extractSingleAsset :: String -> [AssetId]
extractSingleAsset x = [AssetId x]

renderSingleAttribute :: Map AssetId String -> String -> Either Error String
renderSingleAttribute m v =
    case M.lookup (AssetId v) m of
        Just v' -> Right v'
        Nothing -> case M.lookup (AssetId $ tail v) m of
            Just v'' -> Right v''
            Nothing -> Left (UndeclaredDependency (AssetId v))

extractStylesheetAssets :: String -> [AssetId]
extractStylesheetAssets v =
    let Right tokens = tokenize (T.pack v)
    in catMaybes $ (flip map) tokens $ \t ->
            case t of
                (Url x) -> Just $ urlAssetId (T.unpack x)
                _       -> Nothing

renderStylesheetAssets :: Map AssetId String -> String -> Either Error String
renderStylesheetAssets m v = do
    let Right tokens = tokenize (T.pack v)
    newTokens <- forM tokens $ \t -> case t of
        (Url x) -> case M.lookup (urlAssetId (T.unpack x)) m of
            Nothing -> Left (UndeclaredDependency (AssetId (T.unpack x)))
            Just v -> Right (Url $ T.pack $ reconstructUrl (T.unpack x) v)
        _ -> return t

    return $ T.unpack $ serialize newTokens

tagTransformers :: [Transformer]
tagTransformers =
    [ Transformer ("link"==)   "href"    extractSingleAsset renderSingleAttribute
    , Transformer ("img"==)    "src"     extractSingleAsset renderSingleAttribute
    , Transformer ("script"==) "src"     extractSingleAsset renderSingleAttribute
    , Transformer ("a"==)      "href"    extractSingleAsset renderSingleAttribute
    , Transformer ("meta"==)   "content" extractSingleAsset renderSingleAttribute
    , Transformer (const True) "style"   extractStylesheetAssets renderStylesheetAssets
    ]

tagTransformersFor :: String -> [Transformer]
tagTransformersFor tag = filter (\t -> tagSelector t tag) tagTransformers


toInlineStyleDep :: [AssetId] -> [Tag String] -> [AssetId]
toInlineStyleDep acc [] = acc
toInlineStyleDep acc ((TagOpen "style" _):(TagText text):(TagClose "style"):xs)
    = toInlineStyleDep (acc ++ extractStylesheetAssets text) xs
toInlineStyleDep acc (x:xs) = toInlineStyleDep acc xs

renderInlineStyles :: Map AssetId String -> [Tag String] -> Either Error [Tag String]
renderInlineStyles m [] = return []
renderInlineStyles m ((TagOpen "style" attrs):(TagText text):(TagClose "style"):xs) = do
    text' <- renderStylesheetAssets m text
    rest  <- renderInlineStyles m xs
    return $ (TagOpen "style" attrs) : (TagText text') : (TagClose "style") : rest
renderInlineStyles m (x:xs) = do
    xs' <- renderInlineStyles m xs
    return $ x:xs'

htmlBuilder :: String -> String -> Handle -> AssetId -> IO Builder
htmlBuilder pubId src _ _ = do
    body <- readFile src
    let tags = parseTags body
    let deps = concatMap toDep tags
    let inlineStyleDeps = toInlineStyleDep [] tags
    return $ Builder (S.singleton src) (S.fromList (deps ++ inlineStyleDeps)) (render tags)

  where
    toDep :: Tag String -> [AssetId]
    toDep (TagOpen tag attrs) =
        concatMap (\t -> case lookup (attributeName t) attrs of
            Nothing -> []
            Just v ->  depExtractor t v
        ) (tagTransformersFor tag)
    toDep _ = []

    render :: [Tag String] -> Map AssetId String -> Either Error Result
    render tags m = do
        t' <- forM tags $ \t -> do
            insertResult m t

        t'' <- renderInlineStyles m t'

        let body = BS.pack $ renderTags t''
        return $ Result pubId $ Just (body, "text/html")

    insertResult :: Map AssetId String -> Tag String -> Either Error (Tag String)
    insertResult m t@(TagOpen tag attrs) = do
        let tfs = tagTransformersFor tag
        let f at tf = mapM (overrideAttr tf m) at

        TagOpen <$> (pure tag) <*> foldM f attrs tfs

    insertResult _ t = pure t

    overrideAttr :: Transformer -> Map AssetId String -> (String,String) -> Either Error (String,String)
    overrideAttr tf m (k,v)
        | k == attributeName tf = attributeRenderer tf m v >>= \v' -> Right (k, v')
        | otherwise = Right (k,v)
