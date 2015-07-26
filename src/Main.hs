{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Applicative

import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.ByteString as BS

import qualified Data.Text as T

import           Data.Monoid
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

    , optAssets :: [(AssetId,AssetDefinition)]
      -- ^ Additional assets that are specified on the commandline.
    }

data Command
    = Version
    | Build !FilePath
    | Serve


run :: Options -> Command -> IO ()
run _ Version = putStrLn "HEAD"

run opt (Build outputDir) = do
    config <- mkConfig opt outputDir
    h <- newHandle config

    forM_ (entryPoints config) $ \aId ->
        markDirty h aId

    _ <- require h (S.fromList $ entryPoints config)
    atomically $ do
        e <- isEmptyTQueue (messages h)
        unless e retry


run opt Serve = do
    config <- mkConfig opt ""
    h <- newHandle config

    attachFileWatcher h

    -- Mark all entry points as dirty to have the assets ready as soon as
    -- possible, hopefully even before the user opens the web server.
    forM_ (entryPoints config) $ \aId ->
        markDirty h aId

    serveFiles h


collectAssetDefinitions :: FilePath -> FilePath -> IO (Map AssetId AssetDefinition)
collectAssetDefinitions outputDir basePath = do
    FPF.fold (pure True) f M.empty basePath
  where
    f m fi = if takeExtension p == ".html"
        then M.insert
            (AssetId $ drop (length basePath + 1) $ p)
            (AssetDefinition (builderForFile basePath p) transformPublicIdentifierDef (emitResultDef outputDir))
            m
        else m
      where p = infoPath fi


transformPublicIdentifierDef :: PublicIdentifier -> PublicIdentifier
transformPublicIdentifierDef ('/':pubId) = '/' : pubId
transformPublicIdentifierDef pubId       = '/' : pubId


defAutoDiscovery :: Options -> FilePath -> Handle -> AssetId -> IO (Maybe AssetDefinition)
defAutoDiscovery opt outputDir _ (AssetId aId)
    | aId == "" = do
        return $ Just $ AssetDefinition (externalBuilder aId) id (emitResultDef outputDir)
    | isURI aId = do
        return $ Just $ AssetDefinition (externalBuilder aId) id (emitResultDef outputDir)
    | head aId == '/' = do
        -- logMessage h (AssetId aId) $ "Starts with a slash, treating as external!"
        return $ Just $ AssetDefinition (externalBuilder aId) id (emitResultDef outputDir)
    | otherwise = do
        res <- concat <$> mapM (\basePath -> do
                paths <- FPF.find (pure True) f basePath
                return $ zip (repeat basePath) paths
            ) (optPaths opt)
        case sortOn (length . snd) res of
            (basePath, x):_ -> do
                -- logMessage h (AssetId aId) $ "Found asset at " ++ x
                return $ Just $ AssetDefinition (builderForFile basePath x)
                    transformPublicIdentifierDef (emitResultDef outputDir)
            _ -> return Nothing


  where
    f = do
        p <- filePath
        return $ isSuffixOf aId p

emitResultDef :: FilePath -> AssetId -> Result -> IO ()
emitResultDef dist _ (Result pubId mbRes) = do
    case mbRes of
        Nothing -> return ()
        Just (body, _) -> when (dist /= "") $ do
            createDirectoryIfMissing True $ dist `joinDrive` (takeDirectory pubId)
            BS.writeFile (dist `joinDrive` pubId) body

mkConfig :: Options -> FilePath -> IO Config
mkConfig opt outputDir = do
    otherAssets <- mconcat <$> mapM (collectAssetDefinitions outputDir) (optPaths opt)

    let allAssets = M.fromList (optAssets opt) <> otherAssets
    let allEntryPoints = filter (\(AssetId a) -> T.isSuffixOf ".html" (T.pack a)) $ M.keys allAssets
    -- print $ M.keys allAssets

    return $ Config allAssets (defAutoDiscovery opt outputDir) allEntryPoints




parseOptions :: Parser Options
parseOptions = Options <$> parseCommand <*> parsePaths <*> many parseAsset

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
parseServe = pure Serve

assetRead :: ReadM (AssetId, AssetDefinition)
assetRead = ReadM $ do
    v <- ask
    case map T.unpack $ T.splitOn "=" $ T.pack v of
        [aId, p] -> return $ (AssetId aId, AssetDefinition (externalBuilder p) id (\_ _ -> return ()))
        _ -> fail "ASSET=DEFINITION"

parseAsset :: Parser (AssetId, AssetDefinition)
parseAsset = option assetRead
    ( long "asset" <> short 'a' <> metavar "ASSET=DEFINITION" )


parsePaths :: Parser [FilePath]
parsePaths = option pathRead
    ( long "paths" <> short 'p' <> metavar "SEARCH:PATH:DIRS:..."<> value ["assets/"] )
  where
    pathRead = ReadM $ do
        v <- ask
        case map T.unpack $ T.splitOn ":" $ T.pack v of
            (x:xs) -> return $ x:xs
            _ -> fail "SEARCH:PATH:DIRS:..."


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
