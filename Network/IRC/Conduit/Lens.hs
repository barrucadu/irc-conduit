-- |
-- Module      : Network.IRC.Conduit
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- 'Lens'es and 'Prism's.
module Network.IRC.Conduit.Lens where

import           Data.ByteString              (ByteString)
import           Data.Profunctor              (Choice(right'),
                                               Profunctor(dimap))

import           Network.IRC.Conduit.Internal
import           Network.IRC.CTCP             (CTCPByteString)

-- * Lenses for 'Event'

-- | 'Lens' for '_raw'.
raw :: Lens' (Event a) ByteString
{-# INLINE raw #-}
raw afb s = (\b -> s { _raw = b }) <$> afb (_raw s)

-- | 'Lens' for '_source'.
source :: Lens' (Event a) (Source a)
{-# INLINE source #-}
source afb s = (\b -> s { _source = b }) <$> afb (_source s)

-- | 'Lens' for '_message'.
message :: Lens' (Event a) (Message a)
{-# INLINE message #-}
message afb s = (\b -> s { _message = b }) <$> afb (_message s)

-- * Prisms for 'Source'

-- | 'Prism' for 'User'
_User :: Prism' (Source a) (NickName a)
{-# INLINE _User #-}
_User = dimap
  (\s -> case s of User n -> Right n; _ -> Left s)
  (either pure $ fmap User) . right'

-- | 'Prism' for 'Channel'
_Channel :: Prism' (Source a) (ChannelName a, NickName a)
{-# INLINE _Channel #-}
_Channel = dimap
  (\s -> case s of Channel c n -> Right (c,n); _ -> Left s)
  (either pure $ fmap (uncurry Channel)) . right'

-- | 'Prism' for 'Server'
_Server :: Prism' (Source a) (ServerName a)
{-# INLINE _Server #-}
_Server = dimap
  (\s -> case s of Server n -> Right n; _ -> Left s)
  (either pure $ fmap Server) . right'

-- * Prisms for 'Message'

-- | 'Prism' for 'Privmsg'
_Privmsg :: Prism' (Message a) (Target a, Either CTCPByteString a)
{-# INLINE _Privmsg #-}
_Privmsg = dimap
  (\s -> case s of Privmsg t m -> Right (t,m); _ -> Left s)
  (either pure $ fmap (uncurry Privmsg)) . right'

-- | 'Prism' for 'Notice'
_Notice :: Prism' (Message a) (Target a, Either CTCPByteString a)
{-# INLINE _Notice #-}
_Notice = dimap
  (\s -> case s of Notice t m -> Right (t,m); _ -> Left s)
  (either pure $ fmap (uncurry Notice)) . right'

-- | 'Prism' for 'Nick'
_Nick :: Prism' (Message a) (NickName a)
{-# INLINE _Nick #-}
_Nick = dimap
  (\s -> case s of Nick n -> Right n; _ -> Left s)
  (either pure $ fmap Nick) . right'

-- | 'Prism' for 'Join'
_Join :: Prism' (Message a) (ChannelName a)
{-# INLINE _Join #-}
_Join = dimap
  (\s -> case s of Join c -> Right c; _ -> Left s)
  (either pure $ fmap Join) . right'

-- | 'Prism' for 'Part'
_Part :: Prism' (Message a) (ChannelName a, Reason a)
{-# INLINE _Part #-}
_Part = dimap
  (\s -> case s of Part c r -> Right (c,r); _ -> Left s)
  (either pure $ fmap (uncurry Part)) . right'

-- | 'Prism' for 'Quit'
_Quit :: Prism' (Message a) (Reason a)
{-# INLINE _Quit #-}
_Quit = dimap
  (\s -> case s of Quit r -> Right r; _ -> Left s)
  (either pure $ fmap Quit) . right'

-- | 'Prism' for 'Mode'
_Mode :: Prism' (Message a) (Target a, IsModeSet, [ModeFlag a], [ModeArg a])
{-# INLINE _Mode #-}
_Mode = dimap
  (\s -> case s of Mode t i f a -> Right (t,i,f,a); _ -> Left s)
  (either pure $ fmap (\(t,i,f,a) -> Mode t i f a)) . right'

-- | 'Prism' for 'Topic'
_Topic :: Prism' (Message a) (ChannelName a, a)
{-# INLINE _Topic #-}
_Topic = dimap
  (\s -> case s of Topic c t -> Right (c,t); _ -> Left s)
  (either pure $ fmap (uncurry Topic)) . right'

-- | 'Prism' for 'Invite'
_Invite :: Prism' (Message a) (ChannelName a, NickName a)
{-# INLINE _Invite #-}
_Invite = dimap
  (\s -> case s of Invite c n -> Right (c,n); _ -> Left s)
  (either pure $ fmap (uncurry Invite)) . right'

-- | 'Prism' for 'Kick'
_Kick :: Prism' (Message a) (ChannelName a, NickName a, Reason a)
{-# INLINE _Kick #-}
_Kick = dimap
  (\s -> case s of Kick c n r -> Right (c,n,r); _ -> Left s)
  (either pure $ fmap (\(c,n,r) -> Kick c n r)) . right'

-- | 'Prism' for 'Ping'
_Ping :: Prism' (Message a) (ServerName a, Maybe (ServerName a))
{-# INLINE _Ping #-}
_Ping = dimap
  (\s -> case s of Ping x y -> Right (x,y); _ -> Left s)
  (either pure $ fmap (uncurry Ping)) . right'

-- | 'Prism' for 'Pong'
_Pong :: Prism' (Message a) (ServerName a)
{-# INLINE _Pong #-}
_Pong = dimap
  (\s -> case s of Pong x -> Right x; _ -> Left s)
  (either pure $ fmap Pong) . right'

-- | 'Prism' for 'Numeric'
_Numeric :: Prism' (Message a) (Int, [NumericArg a])
{-# INLINE _Numeric #-}
_Numeric = dimap
  (\s -> case s of Numeric n a -> Right (n,a); _ -> Left s)
  (either pure $ fmap (uncurry Numeric)) . right'

-- | 'Prism' for 'RawMsg'
_RawMsg :: Prism' (Message a) a
{-# INLINE _RawMsg #-}
_RawMsg = dimap
  (\s -> case s of RawMsg a -> Right a; _ -> Left s)
  (either pure $ fmap RawMsg) . right'
