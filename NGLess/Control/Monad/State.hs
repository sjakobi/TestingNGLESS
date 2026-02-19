{-# OPTIONS_GHC -O2 #-}

module Control.Monad.State
    ( StateT(..)
    , evalStateT
    , get
    , put
    , modify
    , MonadTrans(..)
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT act) = StateT $ \s ->
        fmap (\(a, s1) -> (f a, s1)) (act s)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    StateT ff <*> StateT fa = StateT $ \s -> do
        (f, s1) <- ff s
        (a, s2) <- fa s1
        pure (f a, s2)

instance Monad m => Monad (StateT s m) where
    StateT fa >>= f = StateT $ \s -> do
        (a, s1) <- fa s
        runStateT (f a) s1

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        pure (a, s)

instance MonadIO m => MonadIO (StateT s m) where
    liftIO io = lift (liftIO io)

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT m s = fmap fst (runStateT m s)

get :: Monad m => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> pure ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> pure ((), f s)
