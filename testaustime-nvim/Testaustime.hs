{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Testaustime (testaustimeHeartBeat, initTestaustimeEnv) where

import GHC.Generics
import Neovim
import Neovim.API.String
import UnliftIO.STM  (TVar, atomically, readTVar, writeTVar, newTVarIO)
import Control.Monad (when)
import Data.Time.Clock.POSIX
import Data.Time
import Data.Aeson (encode, object, (.=), ToJSON)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

data HeartBeat = HeartBeat { project_name :: Maybe String, language :: Maybe String, editor_name :: Maybe String, hostname :: Maybe String} deriving Generic

instance ToJSON HeartBeat

initTestaustimeEnv :: Neovim env (TVar Int)
initTestaustimeEnv = newTVarIO 0

toString :: Neovim.Object -> String
toString (ObjectString x) = unpack x

toMaybe :: String -> Maybe String
toMaybe x
    | x == "" = Nothing
    | otherwise = Just x

secsSinceEpoch :: UTCTime -> Int
secsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

testaustimeHeartBeat :: Neovim (TVar Int) ()
testaustimeHeartBeat = do
    lastUpdatedTVar <- ask
    lastUpdated <- atomically $ do readTVar lastUpdatedTVar
    curTime <- liftIO getCurrentTime
    let curTimeSec = secsSinceEpoch curTime
    when ((curTimeSec-lastUpdated) > 20) $ do
        atomically $ do
            writeTVar lastUpdatedTVar curTimeSec
        hb <- getHeartBeatData
        url <- getUrl
        token <- getAuthorizationToken
        response <- liftIO $ sendHeartBeat hb url token
        return ()

getHeartBeatData :: Neovim env HeartBeat
getHeartBeatData = do
    bf <- nvim_get_current_buf
    isValid <- buffer_is_valid bf
    if isValid then do
        lang <- toString <$> buffer_get_option bf "filetype"
        pName <- reverse . takeWhile (/='/') . reverse . toString <$> nvim_call_function "getcwd" []
        hName <- toString <$> nvim_call_function "hostname" []
        let eName = "NeoVim"
        return (HeartBeat (toMaybe pName) (toMaybe lang) (toMaybe eName) (toMaybe hName))

    else do return (HeartBeat Nothing Nothing Nothing Nothing)

getAuthorizationToken :: Neovim env String
getAuthorizationToken = toString <$> nvim_get_var "testaustime_token"

getUrl :: Neovim env String
getUrl = toString <$> nvim_get_var "testaustime_url"

sendHeartBeat :: HeartBeat -> String -> String -> IO (Response L8.ByteString)
sendHeartBeat hb url token = do
    manager <- newManager tlsManagerSettings
    initRequest <- parseRequest url
    let request = initRequest { method = "POST"
        , requestBody = RequestBodyLBS $ encode hb
        , requestHeaders =
            [ ("Content-Type", "application/json; charset=utf-8")
            , ("Authorization", pack ("Bearer " ++ token))
            ]
        }

    response <- httpLbs request manager
    return response
