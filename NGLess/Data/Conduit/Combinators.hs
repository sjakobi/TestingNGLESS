{-# OPTIONS_GHC -O2 #-}

module Data.Conduit.Combinators
    ( sourceFile
    , takeWhile
    ) where

import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Conduit as C
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (get, put)
import qualified Prelude as P
import Prelude hiding (takeWhile)

sourceFile :: MonadIO m => FilePath -> C.ConduitT i B.ByteString m ()
sourceFile path = do
    txt <- liftIO (readFile path)
    C.yield (P.map (fromIntegral . ord) txt)

takeWhile :: Monad m => (a -> Bool) -> C.ConduitT a a m ()
takeWhile f = C.ConduitT $ do
    xs <- get
    let (prefix, rest) = P.span f xs
    put rest
    pure ((), prefix)
