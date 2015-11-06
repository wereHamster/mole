{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.Html
    ( htmlBuilder
    ) where


import           Control.Monad
import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Text.Encoding as T

import           Data.Maybe
import           Data.CSS.Syntax.Tokens

import           Text.HTML.TagSoup

import           Data.Mole.Types
import           Data.Mole.Builder.Stylesheet



data Transformer = Transformer
    { tagSelector       :: Text -> Bool
      -- ^ Predicate which must return true for the tag to be processed.

    , attributeName     :: Text
      -- ^ Attribute name whose value is processed.

    , depExtractor      :: Text -> [AssetId]
     -- ^ Function which extracts dependencies from the attribute value.

    , attributeRenderer :: Map AssetId PublicIdentifier -> Text -> Either Error Text
      -- ^ Once the dependencies have been resolved, this function is used to
      -- build the new attribute value.
    }

extractSingleAsset :: Text -> [AssetId]
extractSingleAsset x = [AssetId x]

renderSingleAttribute :: Map AssetId PublicIdentifier -> Text -> Either Error Text
renderSingleAttribute m v =
    case M.lookup (AssetId v) m <|> M.lookup (AssetId $ T.tail v) m of
        Just (PublicIdentifier x) -> Right x
        Nothing -> Left (UndeclaredDependency (AssetId v))

extractStylesheetAssets :: Text -> [AssetId]
extractStylesheetAssets v =
    let Right tokens = tokenize v
    in catMaybes $ (flip map) tokens $ \t ->
            case t of
                (Url x) -> Just $ urlAssetId x
                _       -> Nothing

renderStylesheetAssets :: Map AssetId PublicIdentifier -> Text -> Either Error Text
renderStylesheetAssets m v = do
    let Right tokens = tokenize v
    newTokens <- forM tokens $ \t -> case t of
        (Url x) -> case M.lookup (urlAssetId x) m of
            Nothing -> Left (UndeclaredDependency (AssetId x))
            Just (PublicIdentifier v) -> Right (Url $ reconstructUrl x v)
        _ -> return t

    return $ serialize newTokens

tagTransformers :: [Transformer]
tagTransformers =
    [ Transformer ("link"==)   "href"    extractSingleAsset renderSingleAttribute
    , Transformer ("img"==)    "src"     extractSingleAsset renderSingleAttribute
    , Transformer ("script"==) "src"     extractSingleAsset renderSingleAttribute
    , Transformer ("a"==)      "href"    extractSingleAsset renderSingleAttribute
    , Transformer ("meta"==)   "content" extractSingleAsset renderSingleAttribute
    , Transformer (const True) "style"   extractStylesheetAssets renderStylesheetAssets
    ]

tagTransformersFor :: Text -> [Transformer]
tagTransformersFor tag = filter (\t -> tagSelector t tag) tagTransformers


toInlineStyleDep :: [AssetId] -> [Tag Text] -> [AssetId]
toInlineStyleDep acc [] = acc
toInlineStyleDep acc ((TagOpen "style" _):(TagText text):(TagClose "style"):xs)
    = toInlineStyleDep (acc ++ extractStylesheetAssets text) xs
toInlineStyleDep acc (x:xs) = toInlineStyleDep acc xs

renderInlineStyles :: Map AssetId PublicIdentifier -> [Tag Text] -> Either Error [Tag Text]
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
    body <- T.readFile src
    let tags = parseTags body
    let deps = concatMap toDep tags
    let inlineStyleDeps = toInlineStyleDep [] tags
    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.fromList (deps ++ inlineStyleDeps)
        , packageAsset      = render tags
        , sourceFingerprint = T.encodeUtf8 body
        }

  where
    toDep :: Tag Text -> [AssetId]
    toDep (TagOpen tag attrs) =
        concatMap (\t -> case lookup (attributeName t) attrs of
            Nothing -> []
            Just v ->  depExtractor t v
        ) (tagTransformersFor tag)
    toDep _ = []

    render :: [Tag Text] -> Map AssetId PublicIdentifier -> Either Error Result
    render tags m = do
        t' <- forM tags $ \t -> do
            insertResult m t

        t'' <- renderInlineStyles m t'

        let body = T.encodeUtf8 $ renderTags t''
        return $ Result (PublicIdentifier $ T.pack pubId) $ Just (body, "text/html")

    insertResult :: Map AssetId PublicIdentifier -> Tag Text -> Either Error (Tag Text)
    insertResult m t@(TagOpen tag attrs) = do
        let tfs = tagTransformersFor tag
        let f at tf = mapM (overrideAttr tf m) at

        TagOpen <$> (pure tag) <*> foldM f attrs tfs

    insertResult _ t = pure t

    overrideAttr :: Transformer -> Map AssetId PublicIdentifier -> (Text,Text) -> Either Error (Text,Text)
    overrideAttr tf m (k,v)
        | k == attributeName tf = attributeRenderer tf m v >>= \v' -> Right (k, v')
        | otherwise = Right (k,v)
