{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -O2 #-}

module Data.Primitive.Internal.Operations
  ( UnliftedType
  ) where

import GHC.Exts
  ( TYPE
#if __GLASGOW_HASKELL__ >= 902
  , UnliftedType
#endif
  )

#if __GLASGOW_HASKELL__ < 802
type UnliftedType = TYPE 'PtrRepUnlifted
#elif __GLASGOW_HASKELL__ < 902
type UnliftedType = TYPE 'UnliftedRep
#endif
