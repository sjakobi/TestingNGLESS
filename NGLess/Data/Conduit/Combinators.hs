{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Combinators
  ( sourceFile
  , takeWhile
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.Conduit
import           Prelude hiding (takeWhile)
import qualified System.IO as IO

sourceFile :: MonadResource m => FilePath -> ConduitT i ByteString m ()
sourceFile fp =
  bracketP
    (IO.openBinaryFile fp IO.ReadMode)
    IO.hClose
    sourceHandle

sourceHandle :: MonadIO m => IO.Handle -> ConduitT i ByteString m ()
sourceHandle h = loop
  where
    loop = do
      bs <- liftIO $ S.hGetSome h defaultChunkSize
      if S.null bs
        then return ()
        else yield bs >> loop

takeWhile :: Monad m => (a -> Bool) -> ConduitT a a m ()
takeWhile f = loop
  where
    loop = await >>= maybe (return ()) go
    go x
      | f x = yield x >> loop
      | otherwise = leftover x
