{-# LANGUAGE OverloadedStrings #-}

-- |Internal IRC conduit types utilities.
module Network.IRC.Conduit.Internal where

import Control.Applicative ((<$>))
import Data.ByteString     (ByteString, singleton, unpack)
import Data.Char           (ord)
import Data.Maybe          (fromMaybe, isJust)
import Data.Monoid         ((<>))
import Data.String         (fromString)
import Network.IRC.CTCP    (CTCPByteString, getUnderlyingByteString, orCTCP)
import Text.Read           (readMaybe)

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Network.IRC           as I

-- *Type synonyms
type ChannelName a = a
type NickName    a = a
type ServerName  a = a
type Reason      a = a
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
    , _msg     :: Maybe (Message a)
    -- ^The decoded message. If decoding failed, this will be a
    -- Nothing, and the raw bytestring should be consulted.
    , _message :: Message a
    -- ^This will be equal to fromJust _msg, unless _msg is null, in
    -- which case this will be RawMsg. This is provided to make
    -- pattern matching easier.
    }
    deriving (Eq, Show)

instance Functor Event where
    fmap f ev = ev { _source  =      f <$> _source  ev
                   , _msg     = fmap f <$> _msg     ev
                   , _message =      f <$> _message ev
                   }

-- |The source of an IRC message.
data Source a = User (NickName a)
              -- ^The message comes directly from a user.
              | Channel (ChannelName a) (NickName a)
              -- ^The message comes from a user in a channel.
              | Server (ServerName a)
              -- ^The message comes directly from the server.
              deriving (Eq, Show)

instance Functor Source where
    fmap f (User n) = User $ f n
    fmap f (Channel c n) = Channel (f c) $ f n
    fmap f (Server s) = Server $ f s

-- |A decoded IRC message.
data Message a = Privmsg (Target a) (Either CTCPByteString a)
               -- ^A message, either from a user or to a channel the
               -- client is in. CTCPs are distinguished by starting
               -- and ending with a \001 (SOH).
               | Notice (Target a) (Either CTCPByteString a)
               -- ^Like a privmsg, but should not provoke an automatic
               -- response.
               | Nick (NickName a)
               -- ^Someone has updated their nick.
               | Join (ChannelName a)
               -- ^Someone has joined a channel.
               | Part (ChannelName a) (Maybe (Reason a))
               -- ^Someone has left a channel.
               | Quit (Maybe (Reason a))
               -- ^Someone has left the network.
               | Mode (Target a) IsModeSet [ModeFlag a] [ModeArg a]
               -- ^Someone has set some modes channel or user modes.
               | Topic (ChannelName a) a
               -- ^Someone has set the topic of a channel.
               | Invite (ChannelName a) (NickName a)
               -- ^The client has been invited to a channel.
               | Kick (ChannelName a) (NickName a) (Maybe (Reason a))
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
               -- the produced bytestring will be a valid IRC command.
               deriving (Eq, Show)

instance Functor Message where
    fmap f (Privmsg t msg) = Privmsg (f t) $ f <$> msg
    fmap f (Notice  t msg) = Notice  (f t) $ f <$> msg
    fmap f (Nick n) = Nick $ f n
    fmap f (Join c) = Join $ f c
    fmap f (Part c r) = Part (f c) $ f <$> r
    fmap f (Quit r) = Quit $ f <$> r
    fmap f (Mode t ms mf ma) = Mode (f t) ms (map f mf) $ map f ma
    fmap f (Topic c t) = Topic (f c) $ f t
    fmap f (Invite c n) = Invite (f c) $ f n
    fmap f (Kick c n r) = Kick (f c) (f n) $ f <$> r
    fmap f (Ping s1 s2) = Ping (f s1) $ f <$> s2
    fmap f (Pong s) = Pong $ f s
    fmap f (Numeric n na) = Numeric n $ map f na
    fmap f (RawMsg msg) = RawMsg $ f msg

-- *Decoding messages

decode :: ByteString -> Event ByteString
decode bs = Event { _raw     = bs
                  , _source  = source
                  , _msg     = message
                  , _message = fromMaybe (RawMsg bs) message
                  }
    where (source, message) = case I.decode bs of
                                Just ircmsg -> decode' ircmsg
                                -- Fallback for when we can't parse
                                Nothing     -> (Server "", Nothing)

          decode' msg = case msg of
                          -- Disambiguate PRIVMSG and NOTICE source by
                          -- checking the first character of the
                          -- target
                          I.Message (Just (I.NickName n _ _)) "PRIVMSG" [t, m] | isChan t  -> (Channel t n, privmsg t m)
                                                                               | otherwise -> (User n,      privmsg t m)

                          I.Message (Just (I.NickName n _ _)) "NOTICE"  [t, m] | isChan t  -> (Channel t n, notice t m)
                                                                               | otherwise -> (User n,      notice t m)

                          I.Message (Just (I.NickName n _ _)) "NICK"   [n']      -> (User n,      Just $ Nick n')
                          I.Message (Just (I.NickName n _ _)) "JOIN"   [c]       -> (Channel c n, Just $ Join c)
                          I.Message (Just (I.NickName n _ _)) "PART"   [c]       -> (Channel c n, Just $ Part c Nothing)
                          I.Message (Just (I.NickName n _ _)) "PART"   [c, r]    -> (Channel c n, Just $ Part c $ Just r)
                          I.Message (Just (I.NickName n _ _)) "QUIT"   []        -> (User n,      Just $ Quit Nothing)
                          I.Message (Just (I.NickName n _ _)) "QUIT"   [r]       -> (User n,      Just $ Quit $ Just r)
                          I.Message (Just (I.NickName n _ _)) "KICK"   [c, u]    -> (Channel c n, Just $ Kick c u Nothing)
                          I.Message (Just (I.NickName n _ _)) "KICK"   [c, u, r] -> (Channel c n, Just $ Kick c u $ Just r)
                          I.Message (Just (I.NickName n _ _)) "INVITE" [_, c]    -> (User n,      Just $ Invite c n)
                          I.Message (Just (I.NickName n _ _)) "TOPIC"  [c, t]    -> (Channel c n, Just $ Topic  c t)

                          I.Message (Just (I.NickName n _ _)) "MODE" (t:fs:as) | n == t     -> (User n,      mode t fs as)
                                                                               | otherwise -> (Channel t n, mode t fs as)

                          I.Message (Just (I.Server s)) "PING" [s1]     -> (Server s, Just $ Ping s1 Nothing)
                          I.Message (Just (I.Server s)) "PING" [s1, s2] -> (Server s, Just $ Ping s1 $ Just s2)
                          I.Message Nothing             "PING" [s1]     -> (Server s1, Just $ Ping s1 Nothing)
                          I.Message Nothing             "PING" [s1, s2] -> (Server s1, Just $ Ping s1 $ Just s2)

                          I.Message (Just (I.Server s)) n args | isNumeric n -> (Server s, numeric n args)

                          -- Fallback cases
                          I.Message (Just (I.Server s))       _ _ -> (Server s, Nothing)
                          I.Message (Just (I.NickName n _ _)) _ _ -> (User n,   Nothing)
                          _ -> (Server "", Nothing)

          -- An IRC channel name can start with '#', '&', '+', or '!',
          -- all of which ahve different meanings. However, most
          -- servers only support '#'.
          isChan t = B.take 1 t `elem` ["#", "&", "+", "!"]

          -- Check if the message looks like a ctcp or not, and produce the appropriate message type.
          privmsg t = Just . Privmsg t . (Right `orCTCP` Left)
          notice  t = Just . Notice  t . (Right `orCTCP` Left)

          -- Decode a set of mode changes
          mode t fs as = case unpack fs of
                           (f:fs') | f == fromIntegral (ord '+') -> Just $ Mode t True  (map singleton fs') as
                                   | f == fromIntegral (ord '-') -> Just $ Mode t False (map singleton fs') as
                           _ -> Nothing

          -- Parse the number in a numeric response
          isNumeric = isJust . (readMaybe :: String -> Maybe Int) . B8.unpack
          numeric n args = flip Numeric args <$> readMaybe (B8.unpack n)

-- *Encoding messages

encode :: Message ByteString -> ByteString
encode (Privmsg t (Left ctcpbs))  = mkMessage "PRIVMSG" [t, getUnderlyingByteString ctcpbs]
encode (Privmsg t (Right bs))     = mkMessage "PRIVMSG" [t, bs]
encode (Notice  t (Left ctcpbs))  = mkMessage "NOTICE"  [t, getUnderlyingByteString ctcpbs]
encode (Notice  t (Right bs))     = mkMessage "NOTICE"  [t, bs]
encode (Nick n)                   = mkMessage "NICK"    [n]
encode (Join c)                   = mkMessage "JOIN"    [c]
encode (Part c (Just r))          = mkMessage "PART"    [c, r]
encode (Part c Nothing)           = mkMessage "PART"    [c]
encode (Quit (Just r))            = mkMessage "QUIT"    [r]
encode (Quit Nothing)             = mkMessage "QUIT"    []
encode (Mode t True  ms as)       = mkMessage "MODE"    $ t : ("+" <> B.concat ms) : as
encode (Mode t False ms as)       = mkMessage "MODE"    $ t : ("-" <> B.concat ms) : as
encode (Invite c n)               = mkMessage "INVITE"  [c, n]
encode (Topic c bs)               = mkMessage "TOPIC"   [c, bs]
encode (Kick c n (Just r))        = mkMessage "KICK"    [c, n, r]
encode (Kick c n Nothing)         = mkMessage "KICK"    [c, n]
encode (Ping s1 (Just s2))        = mkMessage "PING"    [s1, s2]
encode (Ping s1 Nothing)          = mkMessage "PING"    [s1]
encode (Pong s)                   = mkMessage "PONG"    [s]
encode (Numeric n as)             = mkMessage (fromString $ show n) as
encode (RawMsg bs)                = bs

mkMessage :: ByteString -> [ByteString] -> ByteString
mkMessage cmd = I.encode . I.Message Nothing cmd

-- |Construct a raw message.
rawMessage :: ByteString
           -- ^The command
           -> [ByteString]
           -- ^The arguments
           -> Message ByteString
rawMessage cmd = RawMsg . mkMessage cmd
