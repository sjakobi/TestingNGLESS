{-# OPTIONS_GHC -O2 #-}

module Data.Conduit
    ( ConduitT(..)
    , (.|)
    , await
    , yield
    , yieldMany
    , runConduit
    ) where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (StateT(..), get, put, runStateT)
import Control.Monad.Trans.Class (MonadTrans(..))

newtype ConduitT i o m r = ConduitT { unConduitT :: StateT [i] m (r, [o]) }

instance Functor m => Functor (ConduitT i o m) where
    fmap f (ConduitT act) = ConduitT $ fmap (\(r, out) -> (f r, out)) act

instance Monad m => Applicative (ConduitT i o m) where
    pure r = ConduitT (pure (r, []))
    (<*>) = ap

instance Monad m => Monad (ConduitT i o m) where
    ConduitT fa >>= f = ConduitT $ do
        (a, out1) <- fa
        (b, out2) <- unConduitT (f a)
        pure (b, out1 ++ out2)

instance MonadTrans (ConduitT i o) where
    lift ma = ConduitT $ do
        a <- lift ma
        pure (a, [])

instance MonadIO m => MonadIO (ConduitT i o m) where
    liftIO io = lift (liftIO io)

infixl 1 .|

(.|) :: Monad m => ConduitT a b m r1 -> ConduitT b c m r2 -> ConduitT a c m r2
ConduitT left .| ConduitT right = ConduitT $ StateT $ \input -> do
    ((_, produced), rest) <- runStateT left input
    ((result, produced2), _) <- runStateT right produced
    pure ((result, produced2), rest)

await :: Monad m => ConduitT i o m (Maybe i)
await = ConduitT $ do
    xs <- get
    case xs of
        [] -> pure (Nothing, [])
        (x:rest) -> do
            put rest
            pure (Just x, [])

yield :: Monad m => o -> ConduitT i o m ()
yield o = ConduitT (pure ((), [o]))

yieldMany :: Monad m => [o] -> ConduitT i o m ()
yieldMany out = ConduitT (pure ((), out))

runConduit :: Monad m => ConduitT () o m r -> m r
runConduit (ConduitT act) = do
    ((result, _), _) <- runStateT act []
    pure result
