{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Network.IRC.Conduit.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, DeriveFunctor, OverloadedStrings, RankNTypes, TupleSections
--
-- Internal IRC conduit types and utilities. This module is NOT
-- considered to form part of the public interface of this library.
module Network.IRC.Conduit.Internal where

import           Control.Applicative   ((<$>))
import           Control.Arrow         ((&&&))
import           Data.ByteString       (ByteString, isSuffixOf, singleton,
                                        unpack)
import           Data.Char             (ord)
import           Data.Conduit          (ConduitM, await, yield)
import           Data.Maybe            (isJust, listToMaybe)
import           Data.Monoid           ((<>))
import           Data.Profunctor       (Choice)
import           Data.String           (fromString)
import           Network.IRC.CTCP      (CTCPByteString, getUnderlyingByteString,
                                        orCTCP)
import           Text.Read             (readMaybe)

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Network.IRC           as I

-- * Internal Lens synonyms

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Control.Lens.Lens.Lens>@.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Lens'.
type Lens' s a = Lens s s a a

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#t:Prism Control.Lens.Prism.Prism>@.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Prism'.
type Prism' s a = Prism s s a a


-- *Conduits

-- |Split up incoming bytestrings into new lines.
chunked :: Monad m => ConduitM ByteString ByteString m ()
chunked = chunked' ""
  where
    chunked' !leftover = do
      -- Wait for a value from upstream
      val <- await

      case val of
        Just val' ->
          let
            carriage = fromIntegral $ fromEnum '\r'
            newline  = fromIntegral $ fromEnum '\n'

            -- Split on '\n's, removing any stray '\r's (line endings
            -- are usually '\r\n's, but this isn't certain).
            bytes    = B.filter (/=carriage) $ leftover <> val'
            splitted = B.split newline bytes

            -- If the last chunk ends with a '\n', then we have a
            -- complete message at the end, and can yield it
            -- immediately. Otherwise, store the partial message to
            -- prepend to the next bytestring received.
            (toyield, remainder)
              | singleton newline `isSuffixOf` bytes = (splitted, "")
              | otherwise = init &&& last $ splitted

          in do
            -- Yield all complete and nonempty messages, and loop.
            mapM_ yield $ filter (not . B.null) toyield
            chunked' remainder

        Nothing -> return ()

-- *Type synonyms
type ChannelName a = a
type NickName    a = a
type ServerName  a = a
type Reason      a = Maybe a
type IsModeSet     = Bool
type ModeFlag    a = a
type ModeArg     a = a
type NumericArg  a = a

-- |The target of a message. Will be a nick or channel name.
type Target      a = a

type IrcEvent   = Event ByteString
type IrcSource  = Source ByteString
type IrcMessage = Message ByteString

-- *Messages

-- |A decoded IRC message + source.
data Event a = Event
    { _raw     :: ByteString
    -- ^The message as a bytestring.
    , _source  :: Source a
    -- ^The source of the message (user, channel, or server).
    , _message :: Message a
    -- ^The decoded message. This will never be a 'RawMsg'.
    }
    deriving (Eq, Functor, Show)

-- |The source of an IRC message.
data Source a = User (NickName a)
              -- ^The message comes directly from a user.
              | Channel (ChannelName a) (NickName a)
              -- ^The message comes from a user in a channel.
              | Server (ServerName a)
              -- ^The message comes directly from the server.
              deriving (Eq, Functor, Show)

-- |A decoded IRC message.
data Message a = Privmsg (Target a) (Either CTCPByteString a)
               -- ^A message, either from a user or to a channel the
               -- client is in. CTCPs are distinguished by starting
               -- and ending with a \\001 (SOH).
               | Notice (Target a) (Either CTCPByteString a)
               -- ^Like a privmsg, but should not provoke an automatic
               -- response.
               | Nick (NickName a)
               -- ^Someone has updated their nick.
               | Join (ChannelName a)
               -- ^Someone has joined a channel.
               | Part (ChannelName a) (Reason a)
               -- ^Someone has left a channel.
               | Quit (Reason a)
               -- ^Someone has left the network.
               | Mode (Target a) IsModeSet [ModeFlag a] [ModeArg a]
               -- ^Someone has set some channel modes or user modes.
               | Topic (ChannelName a) a
               -- ^Someone has set the topic of a channel.
               | Invite (ChannelName a) (NickName a)
               -- ^The client has been invited to a channel.
               | Kick (ChannelName a) (NickName a) (Reason a)
               -- ^Someone has been kicked from a channel.
               | Ping (ServerName a) (Maybe (ServerName a))
               -- ^The client has received a server ping, and should
               -- send a pong asap.
               | Pong (ServerName a)
               -- ^A pong sent to the named server.
               | Numeric Int [NumericArg a]
               -- ^One of the many server numeric responses.
               | RawMsg a
               -- ^Never produced by decoding, but can be used to send
               -- arbitrary bytestrings to the IRC server. Naturally,
               -- this should only be used when you are confident that
               -- the produced bytestring will be a valid IRC message.
               deriving (Eq, Functor, Show)

-- *Decoding messages

fromByteString :: ByteString -> Either ByteString IrcEvent
fromByteString bs = maybe (Left bs) (Right . uncurry (Event bs)) (attemptDecode bs)

-- |Attempt to decode a ByteString into a message, returning a Nothing
-- if either the source or the message can't be determined.
attemptDecode :: ByteString -> Maybe (IrcSource, IrcMessage)
attemptDecode bs = I.decode bs >>= decode'
  where
    decode' msg = case msg of
      -- Disambiguate PRIVMSG and NOTICE source by checking the first
      -- character of the target
      I.Message (Just (I.NickName n _ _)) "PRIVMSG" [t, m] | isChan t  -> Just (Channel t n, privmsg t m)
                                                           | otherwise -> Just (User n,      privmsg t m)

      I.Message (Just (I.NickName n _ _)) "NOTICE"  [t, m] | isChan t  -> Just (Channel t n, notice t m)
                                                           | otherwise -> Just (User n,      notice t m)

      I.Message (Just (I.NickName n _ _)) "NICK"   [n']    -> Just (User n,      Nick n')
      I.Message (Just (I.NickName n _ _)) "JOIN"   [c]     -> Just (Channel c n, Join c)
      I.Message (Just (I.NickName n _ _)) "PART"   (c:r)   -> Just (Channel c n, Part c   $ listToMaybe r)
      I.Message (Just (I.NickName n _ _)) "QUIT"   r       -> Just (User n,      Quit     $ listToMaybe r)
      I.Message (Just (I.NickName n _ _)) "KICK"   (c:u:r) -> Just (Channel c n, Kick c u $ listToMaybe r)
      I.Message (Just (I.NickName n _ _)) "INVITE" [_, c]  -> Just (User n,      Invite c n)
      I.Message (Just (I.NickName n _ _)) "TOPIC"  [c, t]  -> Just (Channel c n, Topic  c t)

      I.Message (Just (I.NickName n _ _)) "MODE" (t:fs:as) | n == t    -> (User n,)      <$> mode t fs as
                                                           | otherwise -> (Channel t n,) <$> mode t fs as

      I.Message (Just (I.Server s)) "PING" (s1:s2) -> Just (Server s,  Ping s1 $ listToMaybe s2)
      I.Message Nothing             "PING" (s1:s2) -> Just (Server s1, Ping s1 $ listToMaybe s2)

      I.Message (Just (I.Server s)) n args | isNumeric n -> (Server s,) <$> numeric n args

      _ -> Nothing

    -- An IRC channel name can start with '#', '&', '+', or '!', all
    -- of which have different meanings. However, most servers only
    -- support '#'.
    isChan t = B.take 1 t `elem` ["#", "&", "+", "!"]

    -- Check if the message looks like a ctcp or not, and produce the appropriate message type.
    privmsg t = Privmsg t . (Right `orCTCP` Left)
    notice  t = Notice  t . (Right `orCTCP` Left)

    -- Decode a set of mode changes
    mode t fs as = case unpack fs of
      (f:fs') | f == fromIntegral (ord '+') -> Just $ Mode t True  (map singleton fs') as
              | f == fromIntegral (ord '-') -> Just $ Mode t False (map singleton fs') as
      _ -> Nothing

    -- Parse the number in a numeric response
    isNumeric = isJust . (readMaybe :: String -> Maybe Int) . B8.unpack
    numeric n args = flip Numeric args <$> readMaybe (B8.unpack n)

-- *Encoding messages

-- |Encode an IRC message into a single bytestring suitable for
-- sending to the server.
toByteString :: IrcMessage -> ByteString
toByteString (Privmsg t (Left ctcpbs))  = mkMessage "PRIVMSG" [t, getUnderlyingByteString ctcpbs]
toByteString (Privmsg t (Right bs))     = mkMessage "PRIVMSG" [t, bs]
toByteString (Notice  t (Left ctcpbs))  = mkMessage "NOTICE"  [t, getUnderlyingByteString ctcpbs]
toByteString (Notice  t (Right bs))     = mkMessage "NOTICE"  [t, bs]
toByteString (Nick n)                   = mkMessage "NICK"    [n]
toByteString (Join c)                   = mkMessage "JOIN"    [c]
toByteString (Part c (Just r))          = mkMessage "PART"    [c, r]
toByteString (Part c Nothing)           = mkMessage "PART"    [c]
toByteString (Quit (Just r))            = mkMessage "QUIT"    [r]
toByteString (Quit Nothing)             = mkMessage "QUIT"    []
toByteString (Mode t True  ms as)       = mkMessage "MODE"    $ t : ("+" <> B.concat ms) : as
toByteString (Mode t False ms as)       = mkMessage "MODE"    $ t : ("-" <> B.concat ms) : as
toByteString (Invite c n)               = mkMessage "INVITE"  [c, n]
toByteString (Topic c bs)               = mkMessage "TOPIC"   [c, bs]
toByteString (Kick c n (Just r))        = mkMessage "KICK"    [c, n, r]
toByteString (Kick c n Nothing)         = mkMessage "KICK"    [c, n]
toByteString (Ping s1 (Just s2))        = mkMessage "PING"    [s1, s2]
toByteString (Ping s1 Nothing)          = mkMessage "PING"    [s1]
toByteString (Pong s)                   = mkMessage "PONG"    [s]
toByteString (Numeric n as)             = mkMessage (fromString $ show n) as
toByteString (RawMsg bs)                = bs

mkMessage :: ByteString -> [ByteString] -> ByteString
mkMessage cmd = I.encode . I.Message Nothing cmd

-- |Construct a raw message.
rawMessage :: ByteString
           -- ^The command
           -> [ByteString]
           -- ^The arguments
           -> IrcMessage
rawMessage cmd = RawMsg . mkMessage cmd
