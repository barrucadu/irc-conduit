{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |Internal helper conduits
module Network.IRC.Conduit.Internal.Conduits where

import Control.Arrow          ((&&&))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString, isSuffixOf, singleton)
import Data.Conduit           (Conduit, await, yield)
import Data.Monoid            ((<>))

import qualified Data.ByteString as B

-- |Split up incoming bytestrings into new lines.
chunked :: Monad m => Conduit ByteString m ByteString
chunked = chunked' ""
  where
    chunked' leftover = do
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

-- |Throw an IO exception when the upstream conduit is closed.
exceptionalConduit :: MonadIO m => Conduit a m a
exceptionalConduit = do
  val <- await
  case val of
    Just x  -> yield x >> exceptionalConduit
    Nothing -> liftIO . ioError $ userError "Upstream source closed."
