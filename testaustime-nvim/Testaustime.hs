{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Testaustime (testaustimeHeartBeat, initTestaustimeEnv) where

import GHC.Generics
import Neovim
import Neovim.Exceptions
import Neovim.API.String
import UnliftIO.STM  (TVar, atomically, writeTVar, newTVarIO, readTVarIO)
import Control.Monad (when)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time (UTCTime, nominalDiffTimeToSeconds, getCurrentTime)
import Data.Aeson (encode, object, (.=), ToJSON)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

data HeartBeat = HeartBeat {
    project_name :: Maybe String,
    language :: Maybe String,
    editor_name :: Maybe String,
    hostname :: Maybe String }
    deriving Generic

instance ToJSON HeartBeat

initTestaustimeEnv :: Neovim env (TVar Int)
initTestaustimeEnv = newTVarIO 0

toString :: Neovim.Object -> String
toString (ObjectString x) = unpack x

toMaybe :: String -> Maybe String
toMaybe "" = Nothing
toMaybe x = Just x

fromJustNoFail :: Maybe String -> String
fromJustNoFail (Just x) = x
fromJustNoFail Nothing  = ""

secsSinceEpoch :: UTCTime -> Int
secsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

testaustimeHeartBeat :: Neovim (TVar Int) ()
testaustimeHeartBeat = do
    lastUpdatedTVar <- ask
    lastUpdated <- readTVarIO lastUpdatedTVar
    curTime <- liftIO getCurrentTime
    let curTimeSec = secsSinceEpoch curTime
    when ((curTimeSec-lastUpdated) > 20) $ do
        atomically $ writeTVar lastUpdatedTVar curTimeSec
        hb <- getHeartBeatData
        url <- getUrl
        ignore <- words <$> getIgnoredFiletypes
        token <- getAuthorizationToken
        when (fromJustNoFail (language hb) `notElem` ignore) $ do
            liftIO $ sendHeartBeat hb url token
            return ()

getHeartBeatData :: Neovim env HeartBeat
getHeartBeatData = do
    bf <- nvim_get_current_buf
    isValid <- buffer_is_valid bf
    if isValid then do
        lang <- toString <$> buffer_get_option bf "filetype"
        pName <- reverse . takeWhile (/='/') . reverse . toString <$> nvim_call_function "getcwd" []
        hName <- toString <$> nvim_call_function "hostname" []
        eName <- getEditorName
        return (HeartBeat (toMaybe pName) (toMaybe lang) (toMaybe eName) (toMaybe hName))

    else return (HeartBeat Nothing Nothing Nothing Nothing)

getIgnoredFiletypes :: Neovim env String
getIgnoredFiletypes = getVarIfExists "testaustime_ignore"

getEditorName :: Neovim env String
getEditorName = do
    x <- getVarIfExists "testaustime_editor_name"
    if x == "" then
        return "Neovim"
    else return x

getVarIfExists :: String -> Neovim env String
getVarIfExists x = toString <$> catchNeovimException (nvim_get_var x) handler
    where
        handler :: NeovimException -> Neovim env Object
        handler ex = return (docToObject "")

getAuthorizationToken :: Neovim env String
getAuthorizationToken = toString <$> nvim_get_var "testaustime_token"

getUrl :: Neovim env String
getUrl = toString <$> nvim_get_var "testaustime_url"

sendHeartBeat :: HeartBeat -> String -> String -> IO (Response L8.ByteString)
sendHeartBeat hb url token = do
    manager <- newManager tlsManagerSettings
    initRequest <- parseRequest url
    let request = initRequest {
        method = "POST",
        requestBody = RequestBodyLBS $ encode hb,
        requestHeaders = [
            ("Content-Type", "application/json; charset=utf-8"),
            ("Authorization", pack ("Bearer " ++ token))
            ]
        }

    httpLbs request manager
