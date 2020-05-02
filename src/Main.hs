{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.ByteString as BS

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.List hiding (stripPrefix)

import           Network.URI

import           System.FilePath
import           System.FilePath.Find as FPF
import           System.Directory

import           Data.Mole.Types
import           Data.Mole.Core
import           Data.Mole.Server
import           Data.Mole.Watcher
import           Data.Mole.Builder
import           Data.Mole.Builder.External
import           Data.Mole.Builder.Binary

import           Options.Applicative
import           Options.Applicative.Types



main :: IO ()
main = do
    opt <- execParser (parseOptions `withInfo` "mole")
    run opt (optCommand opt)


data Options = Options
    { optCommand :: Command

    , optPaths :: [FilePath]
      -- ^ The paths where auto-discovery looks for assets.

    , optAssets :: [(AssetId, Options -> String -> IO AssetDefinition)]
      -- ^ Additional assets that are specified on the commandline.
    }

data Command
    = Version
    | Build !FilePath
    | Serve !Int !(Maybe FilePath)


run :: Options -> Command -> IO ()
run _ Version = putStrLn "HEAD"

run opt (Build outputDir) = do
    config <- mkConfig opt outputDir
    h <- newHandle config

    forM_ (entryPoints config) $ \aId ->
        markDirty h aId

    _ <- require h (S.fromList $ entryPoints config)

    aids <- atomically $ do
        s <- readTVar (state h)
        let assetIds = M.keys $ M.filter (\ars -> case arsState ars of Completed _ -> True; Failed _ -> True; _ -> False) (M.restrictKeys (assets s) (S.fromList $ entryPoints config))
        if length assetIds /= (length (entryPoints config)) then retry else pure assetIds

    atomically $ do
        e <- isEmptyTQueue (messages h)
        unless e retry

    atomically $ do
        e <- isEmptyTQueue (emitStream h)
        unless e retry

run opt (Serve port mbSocketPath) = do
    config <- mkConfig opt ""
    h <- newHandle config

    attachFileWatcher h

    -- Mark all entry points as dirty to have the assets ready as soon as
    -- possible, hopefully even before the user opens the web server.
    forM_ (entryPoints config) $ \aId ->
        markDirty h aId

    serveFiles h port mbSocketPath


collectAssetDefinitions :: FilePath -> FilePath -> IO (Map AssetId AssetDefinition)
collectAssetDefinitions outputDir basePath = do
    FPF.fold (pure True) f M.empty basePath
  where
    f m fi = if takeExtension p == ".html"
        then M.insert
            (AssetId $ T.pack $ drop (length basePath + 1) $ p)
            (AssetDefinition (builderForFile basePath p) transformPublicIdentifierDef (emitResultDef outputDir))
            m
        else m
      where p = infoPath fi


transformPublicIdentifierDef :: PublicIdentifier -> PublicIdentifier
transformPublicIdentifierDef pubId = case T.uncons $ unPublicIdentifier pubId of
    Nothing       -> pubId
    Just ('/', _) -> pubId
    _             -> PublicIdentifier $ T.cons '/' $ unPublicIdentifier pubId


locateSource :: Options -> AssetId -> IO (Maybe (String, String))
locateSource opt (AssetId aId) = do
    res <- concat <$> mapM (\basePath -> do
            paths <- FPF.find (pure True) f basePath
            return $ zip (repeat basePath) paths
        ) (optPaths opt)
    case sortOn (length . snd) res of
        (basePath, x):_ -> return $ Just (basePath, x)
        _               -> return Nothing

  where
    f = do
        p <- filePath
        t <- fileType
        return $ t == RegularFile && isSuffixOf (T.unpack aId) p


defAutoDiscovery :: Options -> FilePath -> Handle -> AssetId -> IO (Maybe AssetDefinition)
defAutoDiscovery opt outputDir _ (AssetId aId)
    | aId == "" = do
        return $ Just $ AssetDefinition (externalBuilder $ PublicIdentifier aId) id (emitResultDef outputDir)
    | isURI $ T.unpack aId = do
        return $ Just $ AssetDefinition (externalBuilder $ PublicIdentifier aId) id (emitResultDef outputDir)
    -- | head aId == '/' = do
        -- logMessage h (AssetId aId) $ "Starts with a slash, treating as external!"
        -- return $ Just $ AssetDefinition (externalBuilder aId) id (emitResultDef outputDir)
    | otherwise = do
        mbSource <- locateSource opt (AssetId aId)
        case mbSource of
            Nothing -> return Nothing
            Just (basePath, x) -> do
                -- logMessage h (AssetId aId) $ "Found asset at " ++ x
                return $ Just $ AssetDefinition (builderForFile basePath x)
                    transformPublicIdentifierDef (emitResultDef outputDir)


emitResultDef :: FilePath -> Handle -> AssetId -> Result -> IO ()
emitResultDef dist _ _ (Result pubId mbRes) = do
    case mbRes of
        Nothing -> return ()
        Just (body, _) -> when (dist /= "") $ do
            -- putStrLn $ "emit " ++ pubId
            createDirectoryIfMissing True $ dist `joinDrive` (takeDirectory $ T.unpack $ unPublicIdentifier pubId)
            BS.writeFile (dist `joinDrive` T.unpack (unPublicIdentifier pubId)) body


mkConfig :: Options -> FilePath -> IO Config
mkConfig opt outputDir = do
    otherAssets <- mconcat <$> mapM (collectAssetDefinitions outputDir) (optPaths opt)

    oAssets <- forM (optAssets opt) $ \(aId, m) -> do
        ad <- m opt outputDir
        return (aId, ad)

    let allAssets = M.fromList oAssets <> otherAssets
    let allEntryPoints = filter (\(AssetId a) -> T.isSuffixOf ".html" a) $ M.keys allAssets
    -- print $ M.keys allAssets

    -- let otherAssets = M.fromList $ (flip map) (optEntryPoints opt) $ \(AssetId aId) ->
    --         ( AssetId aId
    --         , AssetDefinition (rawBuilder aId "application/octet-stream" ("_site/" ++ aId)) transformPublicIdentifierDef (emitResultDef outputDir)
    --         )

    return $ Config (allAssets <> otherAssets) (defAutoDiscovery opt outputDir)
        (allEntryPoints ++ (map fst $ optAssets opt))




parseOptions :: Parser Options
parseOptions = (\cmd paths extAssets rawAssets -> Options cmd paths (extAssets <> rawAssets))
    <$> parseCommand
    <*> parsePaths
    <*> many (parseAsset Ext)
    <*> many (parseAsset Raw)

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [ command "version"
        (parseVersion `withInfo` "Print the version and exit")
    , command "build"
        (parseBuild `withInfo` "Build the website")
    , command "serve"
        (parseServe `withInfo` "Serve the website")
    ]

parseVersion :: Parser Command
parseVersion = pure Version

parseBuild :: Parser Command
parseBuild = Build
    <$> strArgument (metavar "OUTPUT-DIRECTORY" <> help "Where to write the files to.")

parseServe :: Parser Command
parseServe = Serve
    <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 8000)
    <*> option (Just <$> str) (long "socket-path" <> short 'u' <> metavar "SOCKET-PATH" <> value Nothing)

askText :: ReaderT String (Except ParseError) Text
askText = T.pack <$> ask

assetIdRead :: ReadM AssetId
assetIdRead = ReadM $ AssetId <$> askText

data AssetType = Ext | Raw

assetRead :: AssetType -> ReadM (AssetId, Options -> String -> IO AssetDefinition)
assetRead at = ReadM $ do
    v <- askText
    case T.splitOn "=" v of
        [aId, p] -> return $ ad aId $ PublicIdentifier p
        _ -> lift $ throwE $ ErrorMsg "ASSET=DEFINITION"

  where
    ad aId p = case at of
        Ext -> (AssetId aId, \_ _ -> return $ AssetDefinition (externalBuilder p) id (\_ _ _ -> return ()))
        Raw ->
            ( AssetId aId
            , \opt outputDir -> do
                mbSource <- locateSource opt (AssetId aId)
                case mbSource of
                    Nothing -> error $ "Could not find asset " ++ T.unpack aId
                    Just (_, p') -> do
                        return $ AssetDefinition (rawBuilder p p' "application/octet-stream") transformPublicIdentifierDef (emitResultDef outputDir)
            )


parseAsset :: AssetType -> Parser (AssetId, Options -> String -> IO AssetDefinition)
parseAsset at = option (assetRead at)
    ( long (prefix <> "-asset") <> metavar "ASSET=DEFINITION" )
  where
    prefix = case at of
        Ext -> "external"
        Raw -> "raw"


parsePaths :: Parser [FilePath]
parsePaths = option pathRead
    ( long "paths" <> short 'p' <> metavar "SEARCH:PATH:DIRS:..."<> value ["assets/"] )
  where
    pathRead = ReadM $ do
        v <- askText
        case map T.unpack $ T.splitOn ":" v of
            (x:xs) -> return $ x:xs
            _ ->  lift $ throwE $ ErrorMsg "SEARCH:PATH:DIRS:..."


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
