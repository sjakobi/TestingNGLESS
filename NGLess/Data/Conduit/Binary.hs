{-# OPTIONS_GHC -O2 #-}

module Data.Conduit.Binary
    ( lines
    ) where

import qualified Data.ByteString as B
import Data.Char (chr, ord)
import qualified Data.Conduit as C
import Control.Monad.State (get, put)
import qualified Prelude as P
import Prelude hiding (lines)

lines :: Monad m => C.ConduitT B.ByteString B.ByteString m ()
lines = C.ConduitT $ do
    chunks <- get
    put []
    let allBytes = concat chunks
    pure ((), splitLines allBytes)

splitLines :: B.ByteString -> [B.ByteString]
splitLines bs = P.map fromString (P.lines (toString bs))
  where
    toString = P.map (chr . fromIntegral)
    fromString = P.map (fromIntegral . ord)
