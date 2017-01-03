{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.IRC.Conduit
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP
--
-- 'Lens'es and 'Prism's.
module Network.IRC.Conduit.Lens where

import Data.ByteString (ByteString)
import Data.Profunctor (Choice (right'), Profunctor (dimap))

import Network.IRC.CTCP (CTCPByteString)
import Network.IRC.Conduit.Internal

-- CPP seem to dislike the first ' on the RHS…
#define PRIME() '

#define LENS(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Lens' for '_/**/F'. -}; \
    F :: Lens' S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)

#define PRISM(S,C,ARG,TUP,A) \
    {-| PRIME()Prism' for 'C'. -}; \
    {-# INLINE _/**/C #-}; \
    _/**/C :: Prism' S A; \
    _/**/C = dimap (\ s -> case s of C ARG -> Right TUP; _ -> Left s) \
        (either pure $ fmap (\ TUP -> C ARG)) . right'

-- * Lenses for 'Event'
LENS((Event a),raw,ByteString)
LENS((Event a),source,(Source a))
LENS((Event a),message,(Message a))

-- * Prisms for 'Source'
PRISM((Source a),User,name,name,(NickName a))
PRISM((Source a),Channel,chan name,(chan,name),(ChannelName a, NickName a))
PRISM((Source a),Server,name,name,(ServerName a))

-- * #Message# Prisms for 'Message'
PRISM((Message a),Privmsg,tar msg,(tar, msg),(Target a, Either CTCPByteString a))
PRISM((Message a),Notice,tar msg,(tar, msg),(Target a, Either CTCPByteString a))
PRISM((Message a),Nick,name,name,(NickName a))
PRISM((Message a),Join,chan,chan,(ChannelName a))
PRISM((Message a),Part,chan reason,(chan, reason),(ChannelName a, Reason a))
PRISM((Message a),Quit,reason,reason,(Reason a))
PRISM((Message a),Mode,tar is flags args,(tar, is, flags, args),(Target a, IsModeSet, [ModeFlag a], [ModeArg a]))
PRISM((Message a),Topic,name topic,(name, topic),(ChannelName a, a))
PRISM((Message a),Invite,chan name,(chan, name),(ChannelName a, NickName a))
PRISM((Message a),Kick,chan name reason,(chan, name, reason),(ChannelName a, NickName a, Reason a))
PRISM((Message a),Ping,ser ver,(ser, ver),(ServerName a, Maybe (ServerName a)))
PRISM((Message a),Pong,ser,ser,(ServerName a))
PRISM((Message a),Numeric,num args,(num, args),(Int, [NumericArg a]))
PRISM((Message a),RawMsg,msg,msg,a)
