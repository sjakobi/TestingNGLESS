{-# LANGUAGE DeriveDataTypeable #-}

module Data.Acquire.Internal
    ( ReleaseType (..)
    ) where

import qualified Control.Exception as E
import           Data.Typeable (Typeable)

-- | The way in which a release is called.
data ReleaseType
    = ReleaseEarly
    | ReleaseNormal
    | ReleaseExceptionWith E.SomeException
    deriving (Show, Typeable)
