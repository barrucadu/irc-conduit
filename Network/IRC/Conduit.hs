{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |Conduits for serialising and deserialising IRC messages.
module Network.IRC.Conduit
    ( -- *Type synonyms
      ChannelName
    , NickName
    , ServerName
    , Reason
    , IsModeSet
    , ModeFlag
    , ModeArg
    , NumericArg
    , Target
    , IrcEvent
    , IrcSource
    , IrcMessage

    -- *Messages
    , Event(..)
    , Source(..)
    , Message(..)

    -- *Conduits
    , ircDecoder
    , ircEncoder
    , floodProtector

    -- *Utilities
    , ircClient
    , ircTlsClient
    , rawMessage
    , toByteString
    ) where

import Control.Applicative      ((*>))
import Control.Arrow            ((&&&))
import Control.Concurrent       (newMVar, takeMVar, putMVar, threadDelay)
import Control.Concurrent.Async (Concurrently(..))
import Control.Monad            (when)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.ByteString          (ByteString, isSuffixOf)
import Data.Conduit             (Conduit, Consumer, Producer, (=$), ($$), (=$=), await, awaitForever, yield)
import Data.Conduit.Network     (AppData, clientSettings, runTCPClient, appSource, appSink)
import Data.Conduit.Network.TLS (tlsClientConfig, runTLSClient)
import Data.Monoid              ((<>))
import Data.Time.Clock          (NominalDiffTime, getCurrentTime, addUTCTime, diffUTCTime)
import Network.IRC.Conduit.Internal
import System.IO.Error          (catchIOError)

import qualified Data.ByteString as B

-- *Conduits

-- |A conduit which takes as input bytestrings representing encoded
-- IRC messages, and decodes them to events.
ircDecoder :: Monad m => Conduit ByteString m IrcEvent
ircDecoder = chunked =$= awaitForever (yield . decode)

-- |Split up incoming bytestrings into new lines.
chunked :: Monad m => Conduit ByteString m ByteString
chunked = chunked' ""
    where chunked' leftover = do
            val <- await 
            case val of
              Just val' ->
                let bytes    = B.filter (/=0o015) $ leftover <> val'
                    splitted = B.split 0o012 bytes

                    (toyield, remainder) = if "\n" `isSuffixOf` bytes
                                           then (splitted, "")
                                           else init &&& last $ splitted

                in mapM_ yield (filter (not . B.null) toyield) >> chunked' remainder

              Nothing -> return ()

-- |Throw an IO exception when the upstream conduit is closed.
exceptionalConduit :: MonadIO m => Conduit a m a
exceptionalConduit = do
  val <- await
  case val of
    Just x  -> yield x >> exceptionalConduit
    Nothing -> liftIO . ioError $ userError "Upstream source closed."

-- |A conduit which as input irc messages, and produces as output the
-- encoded bytestring representation.
ircEncoder :: Monad m => Conduit IrcMessage m ByteString
ircEncoder = awaitForever (yield . (<>"\r\n") . encode)

-- |A conduit which rate limits output sent downstream. Awaiting on
-- this conduit will block, even if there is output ready, until the
-- time limit has passed.
floodProtector :: MonadIO m
               => NominalDiffTime
               -- ^The minimum time between sending adjacent messages.
               -> IO (Conduit a m a)
floodProtector delay = do
  now  <- getCurrentTime
  mvar <- newMVar now

  return $ conduit mvar

  where conduit mvar = awaitForever $ \val -> do
          -- Block until the delay has passed
          liftIO $ do
            lastT <- takeMVar mvar
            now   <- getCurrentTime

            let next = addUTCTime delay lastT

            when (next < now) $
              threadDelay . ceiling $ 1000000 * diffUTCTime next now

          -- Update the time
          liftIO $ do
            now' <- getCurrentTime
            putMVar mvar now'

          -- Send the value downstream
          yield val

-- *Utilities

-- |Connect to a network server, without TLS, and concurrently run the
-- producer and consumer.
ircClient :: Int
          -- ^The port number
          -> ByteString
          -- ^The hostname
          -> IO ()
          -- ^Any initialisation work (started concurrently with the
          -- producer and consumer)
          -> Consumer IrcEvent IO ()
          -- ^The consumer of irc events
          -> Producer IO IrcMessage
          -- ^The producer of irc messages
          -> IO ()
ircClient port host start cons prod = runTCPClient settings $ \a -> networkConduits a start cons prod
    where settings = clientSettings port host

-- |Connect to a network server, with TLS, returning the decoder and
-- encoder conduits.
ircTlsClient :: Int -> ByteString -> IO () -> Consumer IrcEvent IO () -> Producer IO IrcMessage -> IO ()
ircTlsClient port host start cons prod = runTLSClient settings $ \a -> networkConduits a start cons prod
    where settings = tlsClientConfig port host

-- |Construct a pair of conduits from a network connection
networkConduits :: AppData -> IO () -> Consumer IrcEvent IO () -> Producer IO IrcMessage -> IO ()
networkConduits appdata start cons prod = go `catchIOError` const (return ())
    where go = runConcurrently $
                 Concurrently start *>
                 Concurrently (appSource appdata =$= exceptionalConduit $$ ircDecoder =$ cons) *>
                 Concurrently (prod $$ ircEncoder =$ appSink appdata)

-- |Convert a message to a bytestring
toByteString :: IrcMessage -> ByteString
toByteString = encode
