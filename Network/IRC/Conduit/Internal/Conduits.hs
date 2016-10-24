{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Network.IRC.Conduit.Internal.Conduits
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : BangPatterns, OverloadedStrings, RankNTypes
--
-- Internal helper conduits. This module is NOT considered to form
-- part of the public interface of this library.
module Network.IRC.Conduit.Internal.Conduits where

import Control.Arrow   ((&&&))
import Data.ByteString (ByteString, isSuffixOf, singleton)
import Data.Conduit    (Conduit, await, yield)
import Data.Monoid     ((<>))

import qualified Data.ByteString as B

-- |Split up incoming bytestrings into new lines.
chunked :: Monad m => Conduit ByteString m ByteString
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
