module Data.Mole.Builder.Image where


import           Control.Concurrent.STM

import qualified Data.Set as S
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Time
import           Text.Printf

import           Data.Mole.Types
import           Data.Mole.Builder.Internal.Fingerprint

import qualified Network.Kraken as K

import           System.Environment
import           System.FilePath
import           System.Directory
import           System.Posix.Files


imageBuilder :: String -> String -> Handle -> AssetId -> IO Builder
imageBuilder src contentType h aId = do
    originalBody <- BS.readFile src
    let fp = contentHash originalBody
    cacheDir <- lookupEnv "XDG_CACHE_DIR"
    let cacheName = (fromMaybe ".cache" cacheDir) </> "kraken" </> fp
    inCache <- fileExist cacheName
    body <- if inCache
        then do BS.readFile cacheName
        else case krakenH h of
            Nothing -> return originalBody
            Just kh -> do
                atomically $ takeTMVar (lock h)
                logMessage' h aId $ "Compressing image with Kraken..."
                res <- K.compressImage kh (K.Options Nothing Nothing Nothing) originalBody
                case res of
                    Left _ -> do
                        atomically $ putTMVar (lock h) ()
                        return originalBody
                    Right body -> do
                        createDirectoryIfMissing True $ takeDirectory cacheName
                        BS.writeFile cacheName body

                        atomically $ putTMVar (lock h) ()

                        return body

    let ol = BS.length originalBody
    let nl = BS.length body

    let ratio :: Double
        ratio = (100.0 * ((fromIntegral nl :: Double) / (fromIntegral ol)))
    logMessage' h aId $ concat
        [ "Compressed image from "
        , show ol
        , " to "
        , show nl
        , " bytes ("
        , printf "%.2f" ratio
        , "%)"
        ]


    return $ Builder
        { assetSources      = S.singleton src
        , assetDependencies = S.empty
        , packageAsset      = const $ Right $ Result (fingerprint body src) $ Just (body, contentType)
        }


logMessage' :: Handle -> AssetId -> String -> IO ()
logMessage' h aId msg = do
    now <- getCurrentTime
    atomically $ writeTQueue (messages h) (Message now aId msg)
