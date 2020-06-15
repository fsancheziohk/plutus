{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Monad.Freer.WebSocket where

import           Control.Monad.Freer           (Eff, LastMember, type (~>), interpret)
import           Control.Monad.Freer.TH        (makeEffect)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.Aeson                    as JSON
import           Data.UUID                     (UUID)
import           Data.UUID.V4                  (nextRandom)
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (Connection, PendingConnection, receiveData)

data WebSocketEffect r where
    AcceptConnection :: PendingConnection -> WebSocketEffect (UUID, Connection)
    ReceiveJSON :: Connection -> WebSocketEffect (Either String JSON.Value)
    SendJSON :: Connection -> JSON.Value -> WebSocketEffect ()

makeEffect ''WebSocketEffect

handleWebSocket ::
       forall effs m. (LastMember m effs, MonadIO m)
    => Eff (WebSocketEffect ': effs) ~> Eff effs
handleWebSocket =
    interpret $ \case
        ReceiveJSON connection -> do
            msg <- liftIO $ receiveData connection
            pure $ JSON.eitherDecode msg
        SendJSON connection value ->
            liftIO $ WS.sendTextData connection $ JSON.encode value
        AcceptConnection pendingConnection ->
            liftIO $ do
                connection <- WS.acceptRequest pendingConnection
                uuid <- nextRandom
                pure (uuid, connection)
