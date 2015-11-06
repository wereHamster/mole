{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Server where


import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Applicative

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.Monoid
import           Data.Maybe
import           Data.List (find)

import           Snap.Http.Server (simpleHttpServe, ConfigLog(..))
import qualified Snap.Http.Server.Config as SC
import           Snap (Snap, pass, getRequest, rqPathInfo, setContentType, modifyResponse, writeBS)

import           Data.Mole.Types
import           Data.Mole.Core



serveFiles :: Handle -> Int -> Maybe String -> IO ()
serveFiles h port mbSocketPath = do
    snapConfig <- do
        config <- return SC.emptyConfig :: IO (SC.Config Snap ())
        let config' = SC.setAccessLog ConfigNoLog $ SC.setErrorLog ConfigNoLog config
        return $ maybe config' (\x -> SC.setUnixSocket x config') mbSocketPath

    simpleHttpServe (SC.setPort port snapConfig) (snapHandler h)



snapHandler :: Handle -> Snap ()
snapHandler h = do
    req <- getRequest
    serve ("/" <> rqPathInfo req)
        <|> serve ("/" <> rqPathInfo req <> "/index.html")
        <|> serve ("/" <> rqPathInfo req <> "index.html")
        <|> serve "/index.html"
        <|> writeBS ("Asset " <> rqPathInfo req <> " not found")

  where
    serve p = do
      s <- liftIO $ atomically $ readTVar (state h)

      let asts = assetsByPublicIdentifier s (PublicIdentifier $ T.decodeUtf8 p)
    --   liftIO $ print asts
      if length asts == 0
          then pass
          else do
              -- void $ liftIO $ require h $ S.fromList $ map fst asts
              -- void $ liftIO $ require h $ S.singleton $ AssetId $ tail $ unpack p

              let mbRes = find (\res -> isJust (resource res)) $ map snd asts
              case mbRes of
                  Just (Result _ (Just (body, contentType))) -> do
                      modifyResponse $ setContentType (T.encodeUtf8 $ T.pack contentType)
                      writeBS body

                  _ -> pass
