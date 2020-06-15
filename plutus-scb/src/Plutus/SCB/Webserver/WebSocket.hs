{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Plutus.SCB.Webserver.WebSocket
    ( handleWS
    ) where

import           Control.Concurrent.Async      (async, waitAnyCancel)
import           Control.Concurrent.Extra      (threadDelayTime)
import           Control.Exception             (SomeException, handle)
import           Control.Monad                 (forever, void)
import           Control.Monad.Freer           (Eff, LastMember, Member)
import           Control.Monad.Freer.Extra.Log (Log, logInfo)
import           Control.Monad.Freer.Reader    (Reader, ask)
import           Control.Monad.Freer.WebSocket (WebSocketEffect, acceptConnection, receiveJSON, sendJSON)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (LogLevel (LevelDebug))
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSON
import qualified Data.Text                     as Text
import           Data.Time.Units               (Second)
import           Network.WebSockets.Connection (Connection, PendingConnection, withPingThread)
import           Plutus.SCB.App                (runApp)
import           Plutus.SCB.Types              (Config, ContractExe)
import           Plutus.SCB.Utils              (tshow)
import           Plutus.SCB.Webserver.Handler  (getChainReport)
import           Plutus.SCB.Webserver.Types    (StreamToClient (Echo, ErrorResponse, NewChainReport),
                                                StreamToServer (Ping, Request))
import           Wallet.Effects                (ChainIndexEffect)

------------------------------------------------------------
-- Message processors.
------------------------------------------------------------
-- TODO Remove hard-coded `t ~ ContractExe`.
chainReportThread ::
       ( Member ChainIndexEffect effs
       , Member WebSocketEffect effs
       , LastMember m effs
       , MonadIO m
       )
    => Connection
    -> Eff effs ()
chainReportThread connection = do
    chainReport <- getChainReport @ContractExe
    sendJSON connection $ JSON.toJSON $ NewChainReport chainReport
    threadDelayTime (10 :: Second)

chatThread :: Member WebSocketEffect effs => Connection -> Eff effs ()
chatThread connection = do
    payload :: Either String JSON.Value <- receiveJSON connection
    let response :: StreamToClient ContractExe
        response =
            handleStreamingMessage (JSON.parseEither JSON.parseJSON =<< payload)
    sendJSON connection $ JSON.toJSON response

handleStreamingMessage :: Either String (StreamToServer t) -> StreamToClient t
handleStreamingMessage (Left err)         = ErrorResponse $ Text.pack err
handleStreamingMessage (Right (Ping msg)) = Echo msg
handleStreamingMessage (Right Request)    = Echo "ACK"

------------------------------------------------------------
-- Plumbing
------------------------------------------------------------

threadApp ::  Config -> Connection -> IO ()
threadApp config connection =
    void . fmap waitAnyCancel . traverse asyncApp $
    [chainReportThread connection, chatThread connection]
  where
    asyncApp = async . runApp LevelDebug config . forever

handleClient :: Config -> Connection -> IO ()
handleClient config connection =
    handle disconnect . withPingThread connection 30 (pure ()) $
    threadApp config connection
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()

handleWS ::
       ( LastMember m effs
       , MonadIO m
       , Member Log effs
       , Member (Reader Config) effs
       , Member WebSocketEffect effs
       )
    => PendingConnection
    -> Eff effs ()
handleWS pending = do
    (uuid, connection) <- acceptConnection pending
    config <- ask
    logInfo $ "Created connection for user " <> tshow uuid
    liftIO $ handleClient config connection
    logInfo $ "Closed connection for user " <> tshow uuid
