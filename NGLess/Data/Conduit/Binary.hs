module Data.Conduit.Binary
  ( lines
  ) where

import           Control.Monad (unless)
import           Data.Conduit
import qualified Data.ByteString as S
import           Prelude hiding (lines)

lines :: Monad m => ConduitT S.ByteString S.ByteString m ()
lines =
  loop []
  where
    loop acc = await >>= maybe (finish acc) (go acc)

    finish acc =
      let final = S.concat $ reverse acc
       in unless (S.null final) (yield final)

    go acc front =
      case S.elemIndex 10 front of
        Nothing -> loop (front : acc)
        Just i ->
          let (first, second) = S.splitAt i front
              second' = S.drop 1 second
           in yield (S.concat $ reverse (first : acc)) >> go [] second'
