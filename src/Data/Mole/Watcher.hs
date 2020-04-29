{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Watcher
    ( attachFileWatcher
    , detachFileWatcher
    ) where


import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import           Data.Maybe

import           System.FilePath
import           System.Directory
import           System.FSNotify hiding (defaultConfig)

import           Data.Mole.Types
import           Data.Mole.Core



attachFileWatcher :: Handle -> IO ()
attachFileWatcher h = do
    cwd <- getCurrentDirectory
    void $ forkIO $ forever $ do
        withManager $ \mgr -> do
            stop <- watchTree mgr "." (const True) $ \ev -> do
                let p = eventPath ev
                rd <- reverseDependencies h cwd p
                forM_ rd $ \aId -> do
                    logMessage h aId $ "Dirty (" ++ p ++ ")"
                    markDirty h aId

            atomically $ modifyTVar (state h) (\s -> s { stopFileWatcher = stop })

            forever $ threadDelay maxBound


detachFileWatcher :: Handle -> IO ()
detachFileWatcher h = do
    stop <- atomically $ do
        st <- readTVar (state h)
        modifyTVar (state h) (\s -> s { stopFileWatcher = return () })
        return $ stopFileWatcher st

    stop


reverseDependencies :: Handle -> FilePath -> FilePath -> IO [AssetId]
reverseDependencies h cwd p = do
    s <- atomically $ readTVar (state h)
    return $ catMaybes $ (flip map) (M.toList $ assets s) $ \(aId, ars) ->
        if (S.member p $ S.map (\x -> normalise (cwd </> x)) (arsSources ars))
            then Just aId
            else Nothing
