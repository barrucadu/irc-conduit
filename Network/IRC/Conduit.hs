{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Conduit
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : OverloadedStrings, RankNTypes
--
-- Conduits for serialising and deserialising IRC messages.
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
    , ircWithConn
    -- ** TLS
    , ircTLSClient
    , ircTLSClient'
    , defaultTLSConfig

    -- *Utilities
    , rawMessage
    , toByteString

    -- *Lenses
    , module Network.IRC.Conduit.Lens
    ) where

import           Control.Applicative          ((*>))
import           Control.Concurrent           (newMVar, putMVar, takeMVar,
                                               threadDelay)
import           Control.Concurrent.Async     (Concurrently(..))
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.ByteString              (ByteString)
import           Data.Conduit                 (ConduitM, awaitForever,
                                               runConduit, yield, (.|))
import           Data.Conduit.Network         (AppData, appSink, appSource,
                                               clientSettings, runTCPClient)
import           Data.Conduit.Network.TLS     (TLSClientConfig(..),
                                               runTLSClient, tlsClientConfig)
import           Data.Monoid                  ((<>))
import           Data.Text                    (unpack)
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Time.Clock              (NominalDiffTime, addUTCTime,
                                               diffUTCTime, getCurrentTime)
import           Data.Void                    (Void)
import           Data.X509.Validation         (FailedReason(..))
import           Network.Connection           (TLSSettings(..))
import           Network.IRC.Conduit.Internal
import           Network.IRC.Conduit.Lens
import           Network.TLS                  (ClientHooks(..),
                                               ClientParams(..), Supported(..),
                                               Version(..), defaultParamsClient)
import           Network.TLS.Extra            (ciphersuite_strong)

-- *Conduits

-- |A conduit which takes as input bytestrings representing encoded
-- IRC messages, and decodes them to events. If decoding fails, the
-- original bytestring is just passed through.
ircDecoder :: Monad m => ConduitM ByteString (Either ByteString IrcEvent) m ()
ircDecoder = chunked .| awaitForever (yield . fromByteString)

-- |Like 'ircDecoder', but discards messages which could not be
-- decoded.
ircLossyDecoder :: Monad m => ConduitM ByteString IrcEvent m ()
ircLossyDecoder = chunked .| awaitForever lossy
  where
    lossy bs = either (\_ -> return ()) yield $ fromByteString bs

-- |A conduit which takes as input irc messages, and produces as
-- output the encoded bytestring representation.
ircEncoder :: Monad m => ConduitM IrcMessage ByteString m ()
ircEncoder = awaitForever (yield . (<>"\r\n") . toByteString)

-- |A conduit which rate limits output sent downstream. Awaiting on
-- this conduit will block, even if there is output ready, until the
-- time limit has passed.
floodProtector :: MonadIO m
               => NominalDiffTime
               -- ^The minimum time between sending adjacent messages.
               -> IO (ConduitM a a m ())
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
          -> ConduitM (Either ByteString IrcEvent) Void IO ()
          -- ^The consumer of irc events
          -> ConduitM () IrcMessage IO ()
          -- ^The producer of irc messages
          -> IO ()
ircClient port host = ircWithConn $ runTCPClient $ clientSettings port host

-- |Run the IRC conduits using a provided connection.
--
-- Starts the connection and concurrently run the initialiser, event
-- consumer, and message sources. Terminates as soon as one throws an
-- exception.
ircWithConn :: ((AppData -> IO ()) -> IO ())
            -- ^The initialised connection.
            -> IO ()
            -> ConduitM (Either ByteString IrcEvent) Void IO ()
            -> ConduitM () IrcMessage IO ()
            -> IO ()
ircWithConn runner start cons prod = runner $ \appdata -> runConcurrently $
     Concurrently start
  *> Concurrently (runSource appdata)
  *> Concurrently (runSink   appdata)

  where
    runSource appdata  = do
      runConduit $ appSource appdata .| ircDecoder .| cons
      ioError    $ userError "Upstream source closed."

    runSink appdata =
      runConduit $ prod .| ircEncoder .| appSink appdata

-- **TLS

-- |Like 'ircClient', but with TLS. The TLS configuration used is
-- 'defaultTLSConfig'.
ircTLSClient :: Int
             -> ByteString
             -> IO ()
             -> ConduitM (Either ByteString IrcEvent) Void IO ()
             -> ConduitM () IrcMessage IO ()
             -> IO ()
ircTLSClient port host = ircTLSClient' (defaultTLSConfig port host)

-- |Like 'ircTLSClient', but takes the configuration to use, which
-- includes the host and port.
ircTLSClient' :: TLSClientConfig
              -> IO ()
              -> ConduitM (Either ByteString IrcEvent) Void IO ()
              -> ConduitM () IrcMessage IO ()
              -> IO ()
ircTLSClient' cfg = ircWithConn (runTLSClient cfg)

-- |The default TLS settings for 'ircTLSClient'.
defaultTLSConfig :: Int
                 -- ^The port number
                 -> ByteString
                 -- ^ The hostname
                 -> TLSClientConfig
defaultTLSConfig port host = (tlsClientConfig port host)
  { tlsClientTLSSettings = TLSSettings cpara
    { clientHooks = (clientHooks cpara)
      { onServerCertificate = validate }
    , clientSupported = (clientSupported cpara)
      { supportedVersions = [TLS12, TLS11, TLS10]
      , supportedCiphers = ciphersuite_strong
      }
    }
  }

  where
    cpara = defaultParamsClient (unpack $ decodeUtf8 host) ""

    -- Make the TLS certificate validation a bit more generous. In
    -- particular, allow self-signed certificates.
    validate cs vc sid cc = do
      -- First validate with the standard function
      res <- (onServerCertificate $ clientHooks cpara) cs vc sid cc
      -- Then strip out non-issues
      return $ filter (`notElem` [UnknownCA, SelfSigned]) res
