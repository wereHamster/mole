module Data.Mole.Core where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Maybe

import           Data.Map (Map)
import qualified Data.Map as M

import           Data.Set (Set)
import qualified Data.Set as S

import           Data.ByteString (ByteString)

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


    -- This background thread periodically checks if there are any assets
    -- marked as dirty and forks a build thread for each.
    tId <- forkIO $ forever $ do

        -- Get a list of dirty assets. Those are the ones which we need to
        -- rebuild. The check runs in a STM transaction, and will block until
        -- at least one asset is dirty. Much efficient, wow.
        dirtyAssetIds <- atomically $ do
            s <- readTVar st
            let assetIds = M.keys $ M.filter ((==) Dirty . arsState) (assets s)
            if length assetIds == 0 then retry else return assetIds

        forM_ dirtyAssetIds $ \aId -> do
            -- First we have to mark the asset as being built. This is to avoid
            -- forking two or more build threads for the same asset.
            markBuilding h aId

            forkIO $ do
                assetDef <- lookupAssetDefinition config h aId
                case assetDef of
                    Nothing -> do
                        -- failBuild h aId (AssetNotFound aId)
                        logMessage h aId $ "Asset not found, treating as external: " ++ show aId
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


adjustAssetRuntimeState :: Handle -> AssetId -> (AssetRuntimeState -> AssetRuntimeState) -> IO ()
adjustAssetRuntimeState h aId f = atomically $ do
    modifyTVar (state h) $ \s -> s { assets = M.adjust f aId (assets s) }

insertAssetRuntimeStateWith :: Handle -> AssetId -> (AssetRuntimeState -> AssetRuntimeState -> AssetRuntimeState) -> AssetRuntimeState -> IO ()
insertAssetRuntimeStateWith h aId f d = atomically $ do
    modifyTVar (state h) $ \s -> s { assets = M.insertWith f aId d (assets s) }


updateMetadata :: Handle -> AssetId -> Set FilePath -> Set AssetId -> ByteString -> IO ()
updateMetadata h aId src ds fp = adjustAssetRuntimeState h aId $ \ars -> ars
    { arsSources = src
    , arsDependencySet = ds
    , arsSourceFingerprint = Just fp
    }


buildIfNecessary :: Handle -> AssetId -> IO ()
buildIfNecessary h aId = insertAssetRuntimeStateWith h aId adj (assetRuntimeState Dirty)
  where
    adj _ ars = case arsState ars of
        Building _  -> ars
        Completed _ -> ars
        _           -> ars { arsState = Dirty }


markDirty :: Handle -> AssetId -> IO ()
markDirty h aId = insertAssetRuntimeStateWith h aId f (assetRuntimeState Dirty)
  where f _ ars = ars { arsState = Dirty }


markBuilding :: Handle -> AssetId -> IO ()
markBuilding h aId = do
    s <- Building <$> getCurrentTime
    insertAssetRuntimeStateWith h aId (\_ ars -> ars { arsState = s }) (assetRuntimeState s)


failBuild :: Handle -> AssetId -> Error -> IO ()
failBuild h aId err = do
    logMessage h aId $ "Failure: " ++ show err
    adjustAssetRuntimeState h aId $ \ars -> ars { arsState = Failed err }
    rebuildReverseDependencies h aId

finishBuilding :: Handle -> AssetId -> Result -> IO ()
finishBuilding h aId res = do
    now <- getCurrentTime

    let diff (Building t0) = diffUTCTime now t0
        diff _             = fromIntegral (0 :: Int)

    adjustAssetRuntimeState h aId $ \ars -> ars
        { arsState = Completed (diff $ arsState ars)
        , arsResult = Just res
        }

    mbArs <- atomically $ do
        s <- readTVar (state h)
        return $ M.lookup aId (assets s)

    case mbArs of
        Nothing -> return ()
        Just ars -> case arsState ars of
            Completed td -> logMessage h aId $ "Build time: " ++ show td
            _ -> return ()


    -- Go through all reverse dependencies and mark them as dirty.
    rebuildReverseDependencies h aId


isBuilding :: AssetState -> Bool
isBuilding (Building _) = True
isBuilding _            = False

isFailed :: AssetState -> Bool
isFailed (Failed _) = True
isFailed _          = False

rebuildReverseDependencies :: Handle -> AssetId -> IO ()
rebuildReverseDependencies h aId = do
    s <- atomically $ readTVar (state h)
    forM_ (M.toList $ assets s) $ \(aId', ars) -> do
        when ((not $ isBuilding $ arsState ars) && S.member aId (arsDependencySet ars)) $ do
            markDirty h aId'


-- | Wait until the set of assets is built, and return the corresponding
-- results. If any of the assets fails to build (for whatever reason), then
-- immediately abort and return the reason.
require :: Handle -> Set AssetId -> IO (Either Error (Map AssetId Result))
require h assetIds = do
    -- Mark assets as dirty if they are not comleted yet.
    forM_ assetIds $ \dep -> do
        buildIfNecessary h dep

    -- Wait for the dependencies to have completed building.
    atomically $ do
        s <- readTVar (state h)

        -- All dependencies which are relevant.
        let allDependencies = M.filterWithKey (\aId _ -> S.member aId assetIds) (assets s)

        -- The dependencies which are completed and for which we have a result.
        let completedDependencies = flip M.mapMaybe allDependencies $ \ars -> case (arsState ars, arsResult ars) of
                (Completed _, Just res) -> Just res
                _                       -> Nothing

        -- A more accurate check would be 'assetIds == M.keysSet completedDependencies'.
        -- Though comparing the length is probably faster.
        if length completedDependencies == length assetIds
            then return $ Right $ completedDependencies
            else if any (isFailed . arsState) (M.elems allDependencies)
                then return $ Left DependencyFailed
                else retry


assetsByPublicIdentifier :: State -> PublicIdentifier -> [(AssetId, Result)]
assetsByPublicIdentifier st pubId = filter (\(_,res) -> publicIdentifier res == pubId) $
    catMaybes $ map f $ M.assocs $ assets st
  where f (aId, AssetRuntimeState (Completed _) _ _ _ (Just res)) = Just (aId, res)
        f _ = Nothing

assetByPublicIdentifier :: State -> PublicIdentifier -> Maybe Result
assetByPublicIdentifier st pubId = lookup pubId $ catMaybes $ map f $ M.elems $ assets st
  where f (AssetRuntimeState (Completed _) _ _ _ (Just res)) = Just (publicIdentifier res, res)
        f _ = Nothing



lookupAssetDefinition :: Config -> Handle -> AssetId -> IO (Maybe AssetDefinition)
lookupAssetDefinition config h aId = case M.lookup aId (assetDefinitions config) of
    Just ad -> return $ Just ad
    Nothing -> autoDiscovery config h aId


buildAsset :: Handle -> AssetId -> AssetDefinition -> IO ()
buildAsset h aId ad = do
    Builder src depSet cont fp <- createBuilder ad h aId

    -- First check if we actually need to rebuild the asset. If the source
    -- fingerprint is still the same then we can skip directly to 'Completed'.
    needsRebuild <- atomically $ do
        s <- readTVar (state h)
        return $ case M.lookup aId (assets s) of
            Just (AssetRuntimeState _ _ _ (Just sfp) (Just _)) -> sfp /= fp
            _                                                  -> True

    -- Eagerly update the metadata, even if we don't have to rebuild the asset.
    -- When deciding whether to rebuild the asset or not, the only thing that
    -- matters is the fingerprint. But the builder may have an updated or more
    -- accurate set of dependencies now, and we do want to update that.
    updateMetadata h aId src depSet fp


    if not needsRebuild
        then do
            logMessage h aId $ "Skip"

            now <- getCurrentTime

            let diff (Building t0) = diffUTCTime now t0
                diff _             = fromIntegral (0 :: Int)

            adjustAssetRuntimeState h aId $ \ars ->
                ars { arsState = Completed (diff $ arsState ars) }

        else do
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
