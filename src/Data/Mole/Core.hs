module Data.Mole.Core where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Maybe

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Maybe
import           Data.Monoid
import           Data.Time

import           Data.Mole.Types
import           Data.Mole.Builder.External

import           System.Environment
import           System.IO (hFlush, stdout)

import qualified Network.Kraken as K



padL :: Int -> String -> String
padL n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

newHandle :: Config -> IO Handle
newHandle config = do
    st <- newTVarIO $ State Nothing (return ()) M.empty
    l <- newTMVarIO ()

    msgs <- newTQueueIO
    void $ forkIO $ forever $ do
        (Message time aId msg) <- atomically $ readTQueue msgs
        putStrLn $ mconcat
            [ formatTime defaultTimeLocale "%H:%M:%S" time
            , " [ " <> take 24 (padL 24 (unAssetId aId)) <> " ] "
            , msg
            ]
        hFlush stdout

    e <- newTQueueIO
    void $ forkIO $ forever $ do
        join $ atomically $ readTQueue e

    kH <- runMaybeT $ do
        apiKey <- MaybeT $ lookupEnv "KRAKEN_API_KEY"
        apiSecret <- MaybeT $ lookupEnv "KRAKEN_API_SECRET"

        MaybeT $ Just <$> K.newHandle (K.Config apiKey apiSecret)

    let h = Handle st msgs e kH l

    tId <- forkIO $ forever $ do
        da <- dirtyAssets st
        forM_ da $ \aId -> do
            -- logger lock $ "Asset " ++ (show aId) ++ " is dirty. Building..."
            markBuilding h aId
            forkIO $ do
                assetDef <- lookupAssetDefinition config h aId
                case assetDef of
                    Nothing -> do -- failBuild h aId (AssetNotFound aId)
                        logMessage h aId $ "Asset not found, treating as external!: " ++ show aId
                        buildAsset h aId $ AssetDefinition (externalBuilder $ unAssetId aId) id (\_ _ _ -> return ())
                    Just ad -> do
                        -- logMessage h aId $ "Building"
                        buildAsset h aId ad

    atomically $ modifyTVar st (\s -> s { dispatcherThreadId = Just tId })

    return h

logMessage :: Handle -> AssetId -> String -> IO ()
logMessage h aId msg = do
    now <- getCurrentTime
    atomically $ writeTQueue (messages h) (Message now aId msg)


updateMetadata :: Handle -> AssetId -> Set FilePath -> Set AssetId -> IO ()
updateMetadata h aId src ds = atomically $ do
    modifyTVar (state h) $ \s -> s { assets = M.adjust (\ars -> ars {
        arsSources = src, arsDependencySet = ds }) aId (assets s) }


buildIfNecessary :: Handle -> AssetId -> IO ()
buildIfNecessary h aId = atomically $ do
    modifyTVar (state h) $ \s -> s { assets = M.insertWith adj aId (AssetRuntimeState Dirty S.empty S.empty) (assets s) }
  where
    adj _ ars = case arsState ars of
        Building _ -> ars
        Completed _ _ -> ars
        _        -> ars { arsState = Dirty }


markDirty :: Handle -> AssetId -> IO ()
markDirty h aId = atomically $ do
    modifyTVar (state h) $ \s -> s { assets = M.insertWith (\_ ars -> ars { arsState = Dirty }) aId (AssetRuntimeState Dirty S.empty S.empty) (assets s) }


markBuilding :: Handle -> AssetId -> IO ()
markBuilding h aId = do
    now <- getCurrentTime
    atomically $ do
        modifyTVar (state h) $ \s -> s { assets = M.insertWith (\_ ars -> ars { arsState = Building now }) aId (AssetRuntimeState (Building now) S.empty S.empty) (assets s) }


failBuild :: Handle -> AssetId -> Error -> IO ()
failBuild h aId err = do
    logMessage h aId $ "Failure: " ++ show err
    atomically $ do
        modifyTVar (state h) $ \s -> s { assets = M.adjust (\ars -> ars { arsState = Failed err }) aId (assets s) }

    rebuildReverseDependencies h aId

finishBuilding :: Handle -> AssetId -> Result -> IO ()
finishBuilding h aId res = do
    now <- getCurrentTime

    let diff (Building t0) = diffUTCTime now t0
        diff _             = fromIntegral (0 :: Int)

    atomically $ do
        modifyTVar (state h) $ \s -> s { assets = M.adjust (\ars -> ars { arsState = Completed res (diff $ arsState ars) }) aId (assets s) }

    -- Go through all reverse dependencies and mark them as dirty.
    rebuildReverseDependencies h aId


isBuilding :: AssetState -> Bool
isBuilding (Building _) = True
isBuilding _            = False

rebuildReverseDependencies :: Handle -> AssetId -> IO ()
rebuildReverseDependencies h aId = do
    s <- atomically $ readTVar (state h)
    forM_ (M.toList $ assets s) $ \(aId', ars) -> do
        when ((not $ isBuilding $ arsState ars) && S.member aId (arsDependencySet ars)) $ do
            markDirty h aId'


require :: Handle -> Set AssetId -> IO (Either Error (Map AssetId Result))
require h assetIds = do
    -- Mark assets as dirty if they are not comleted yet.
    forM_ (S.toList assetIds) $ \dep -> do
        buildIfNecessary h dep

    -- Wait for the dependencies to have completed building.
    atomically $ do
        s <- readTVar (state h)

        let de = filter (\(aId, _) -> S.member aId assetIds) (M.toList (assets s))
        let completedPubRefs = catMaybes $ map (\(aId, ars) -> case (arsState ars) of
                Completed res _ -> Just (aId, res)
                _ -> Nothing) de

        if length completedPubRefs == length assetIds
            then return $ Right $ M.fromList completedPubRefs
            else if any (\(_, ars) -> case arsState ars of Failed _ -> True; _ -> False) de
                then return $ Left DependencyFailed
                else retry

assetsByPublicIdentifier :: State -> PublicIdentifier -> [(AssetId, Result)]
assetsByPublicIdentifier st pubId = filter (\(_,res) -> publicIdentifier res == pubId) $
    catMaybes $ map f $ M.assocs $ assets st
  where f (aId, AssetRuntimeState (Completed res _) _ _) = Just (aId, res)
        f _ = Nothing

assetByPublicIdentifier :: State -> PublicIdentifier -> Maybe Result
assetByPublicIdentifier st pubId = lookup pubId $ catMaybes $ map f $ M.elems $ assets st
  where f (AssetRuntimeState (Completed res _) _ _) = Just (publicIdentifier res, res)
        f _ = Nothing




dirtyAssets :: TVar State -> IO [AssetId]
dirtyAssets st = atomically $ do
    s <- readTVar st
    let de = filter (\(_, ars) -> Dirty == arsState ars) $ M.toList (assets s)
    if length de == 0
        then retry
        else return $ map fst de

lookupAssetDefinition :: Config -> Handle -> AssetId -> IO (Maybe AssetDefinition)
lookupAssetDefinition config h aId = case M.lookup aId (assetDefinitions config) of
    Just ad -> return $ Just ad
    Nothing -> autoDiscovery config h aId


buildAsset :: Handle -> AssetId -> AssetDefinition -> IO ()
buildAsset h aId ad = do
    Builder src depSet cont <- createBuilder ad h aId

    updateMetadata h aId src depSet

    -- putStrLn $ "Waiting for " ++ show depSet
    rd <- require h depSet
    case rd of
        Left e -> failBuild h aId e
        Right resolvedDeps -> do
            -- logger lock $ "Got all dependencies of " ++ show aId
            -- logger lock $ resolvedDeps
            case cont (M.map publicIdentifier resolvedDeps) of
                Left e -> failBuild h aId e
                Right result1@(Result pub _) -> do
                    let result = result1 { publicIdentifier = transformPublicIdentifier ad pub }
                    -- logger lock $ "Pub: " ++ (publicIdentifier result)
                    -- logger lock $ res

                    atomically $ writeTQueue (emitStream h) $ emitResult ad h aId result
                    finishBuilding h aId result
