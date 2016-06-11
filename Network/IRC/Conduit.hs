{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |Conduits for serialising and deserialising IRC messages.
--
-- The 'Event', 'Message', and 'Source' types are parameterised on the
-- underlying representation, and are functors. Decoding and encoding
-- only work in terms of 'ByteString's, but the generality is provided
-- so that programs using this library can operate in terms of 'Text',
-- or some other more useful representation, with great ease.
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
    , ircLossyDecoder
    , ircEncoder
    , floodProtector

    -- *Networking
    , ircClient
    , ircTLSClient
    , ircWithConn

    -- *Utilities
    , rawMessage
    , toByteString
    ) where

import Control.Applicative      ((*>))
import Control.Concurrent       (newMVar, takeMVar, putMVar, threadDelay)
import Control.Concurrent.Async (Concurrently(..))
import Control.Exception        (catch)
import Control.Monad            (when)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.ByteString          (ByteString)
import Data.Conduit             (Conduit, Consumer, Producer, (=$), ($$), (=$=), awaitForever, yield)
import Data.Conduit.Network     (AppData, clientSettings, runTCPClient, appSource, appSink)
import Data.Conduit.Network.TLS (TLSClientConfig(..), tlsClientConfig, runTLSClient)
import Data.Monoid              ((<>))
import Data.Text                (unpack)
import Data.Text.Encoding       (decodeUtf8)
import Data.Time.Clock          (NominalDiffTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.X509.Validation     (FailedReason(..))
import Network.Connection       (TLSSettings(..))
import Network.IRC.Conduit.Internal
import Network.TLS              (ClientParams(..), ClientHooks(..), Supported(..), TLSException, Version(..), defaultParamsClient)
import Network.TLS.Extra        (ciphersuite_all)
import System.IO.Error          (catchIOError)

-- *Conduits

-- |A conduit which takes as input bytestrings representing encoded
-- IRC messages, and decodes them to events. If decoding fails, the
-- original bytestring is just passed through.
ircDecoder :: Monad m => Conduit ByteString m (Either ByteString IrcEvent)
ircDecoder = chunked =$= awaitForever (yield . fromByteString)

-- |Like 'ircDecoder', but discards messages which could not be
-- decoded.
ircLossyDecoder :: Monad m => Conduit ByteString m IrcEvent
ircLossyDecoder = chunked =$= awaitForever lossy
  where
    lossy bs = either (\_ -> return ()) yield $ fromByteString bs

-- |A conduit which takes as input irc messages, and produces as
-- output the encoded bytestring representation.
ircEncoder :: Monad m => Conduit IrcMessage m ByteString
ircEncoder = awaitForever (yield . (<>"\r\n") . toByteString)

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

  where
    conduit mvar = awaitForever $ \val -> do
      -- Block until the delay has passed
      liftIO $ do
        lastT <- takeMVar mvar
        now   <- getCurrentTime

        let next = addUTCTime delay lastT

        when (now < next) $
          threadDelay . ceiling $ 1000000 * diffUTCTime next now

      -- Update the time
        now' <- getCurrentTime
        putMVar mvar now'

      -- Send the value downstream
      yield val

-- *Networking

-- |Connect to a network server, without TLS, and concurrently run the
-- producer and consumer.
ircClient :: Int
          -- ^The port number
          -> ByteString
          -- ^The hostname
          -> IO ()
          -- ^Any initialisation work (started concurrently with the
          -- producer and consumer)
          -> Consumer (Either ByteString IrcEvent) IO ()
          -- ^The consumer of irc events
          -> Producer IO IrcMessage
          -- ^The producer of irc messages
          -> IO ()
ircClient port host = ircWithConn $ runTCPClient $ clientSettings port host

-- |Like 'ircClient', but with TLS.
ircTLSClient :: Int -> ByteString -> IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()
ircTLSClient port host = ircWithConn $ runTLSClient $ mangle $ tlsClientConfig port host
  where
    -- Override the certificate validation and allowed ciphers.
    mangle tlsSettings = tlsSettings
                         { tlsClientTLSSettings = TLSSettings cpara
                           { clientHooks = (clientHooks cpara)
                             { onServerCertificate = validate }
                           , clientSupported = (clientSupported cpara)
                             { supportedVersions = [TLS12, TLS11, TLS10]
                             , supportedCiphers = ciphersuite_all }}}
      where
        cpara = defaultParamsClient (unpack $ decodeUtf8 host) ""

        -- Make the TLS certificate validation a bit more generous. In
        -- particular, allow self-signed certificates.
        validate cs vc sid cc = do
          -- First validate with the standard function
          res <- (onServerCertificate $ clientHooks cpara) cs vc sid cc
          -- Then strip out non-issues
          return $ filter (`notElem` [UnknownCA, SelfSigned]) res

-- |Run the IRC conduits using a provided connection.
ircWithConn :: ((AppData -> IO ()) -> IO ())
            -- ^The initialised connection.
            -> IO ()
            -> Consumer (Either ByteString IrcEvent) IO ()
            -> Producer IO IrcMessage
            -> IO ()
ircWithConn runner start cons prod = (go `catch` raiseTLS) `catchIOError` ignore
  where
    -- Start the connection and concurrently run the initialiser,
    -- event consumer, and message sources: terminating as soon as one
    -- throws an exception.
    go = runner $ \appdata ->
           runConcurrently $
             Concurrently start *>
             Concurrently (appSource appdata =$= exceptionalConduit $$ ircDecoder =$ cons) *>
             Concurrently (prod $$ ircEncoder =$ appSink appdata)

    -- Ignore all exceptions and just halt.
    ignore _ = return ()

    -- Rethrow TLS exceptions as IO exceptions
    raiseTLS = const . ioError $ userError "TLS exception" :: TLSException -> IO ()
