{-# OPTIONS_GHC -O2 #-}

module Control.Monad.Trans.Class
    ( MonadTrans(..)
    ) where

class MonadTrans t where
    lift :: Monad m => m a -> t m a
