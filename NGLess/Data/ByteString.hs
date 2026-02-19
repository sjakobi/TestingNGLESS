{-# OPTIONS_GHC -O2 #-}

module Data.ByteString
    ( ByteString
    , null
    , head
    ) where

import Prelude hiding (head, null)
import qualified Prelude as P
import Data.Word (Word8)

type ByteString = [Word8]

null :: ByteString -> Bool
null = P.null

head :: ByteString -> Word8
head = P.head
