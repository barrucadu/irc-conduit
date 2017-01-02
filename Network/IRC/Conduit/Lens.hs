{-# LANGUAGE CPP #-}

-- | 'Lens'es.
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

-- makeLenses ''Event
LENS((Event a),raw,ByteString)
LENS((Event a),source,(Source a))
LENS((Event a),message,(Message a))

