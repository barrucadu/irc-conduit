{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.IRC.Conduit
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP
--
-- Lenses.
module Network.IRC.Conduit.Lens where

import Data.ByteString (ByteString)

import Network.IRC.Conduit.Internal

-- CPP seem to dislike the first ' on the RHS…
#define PRIME() '

#define LENS(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Lens' for '_/**/F'. -}; \
    F :: Lens' S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)

-- * 'Event'

-- makeLenses ''Event
LENS((Event a),raw,ByteString)
LENS((Event a),source,(Source a))
LENS((Event a),message,(Message a))
