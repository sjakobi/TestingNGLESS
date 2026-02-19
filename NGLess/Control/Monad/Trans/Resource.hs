{-# OPTIONS_GHC -O2 #-}

module Control.Monad.Trans.Resource
    ( ResourceT(..)
    , runResourceT
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

newtype ResourceT m a = ResourceT { unResourceT :: m a }

runResourceT :: ResourceT m a -> m a
runResourceT = unResourceT

instance Functor m => Functor (ResourceT m) where
    fmap f (ResourceT ma) = ResourceT (fmap f ma)

instance Applicative m => Applicative (ResourceT m) where
    pure = ResourceT . pure
    ResourceT mf <*> ResourceT ma = ResourceT (mf <*> ma)

instance Monad m => Monad (ResourceT m) where
    ResourceT ma >>= f = ResourceT $ do
        a <- ma
        unResourceT (f a)

instance MonadTrans ResourceT where
    lift = ResourceT

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO
