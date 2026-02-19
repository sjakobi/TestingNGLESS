{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Deps
  ( ResourceT
  , runResourceT
  , runConduit
  , await
  , sourceFile
  , takeWhile
  , lines
  , (.|)
  ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Concurrent ()
import Control.Exception
    ( IOException, throw, Exception, SomeException )
import Control.Monad ( ap, liftM, MonadPlus(..), unless )
import Control.Monad.Catch
    ( MonadThrow(..), MonadCatch(..), MonadMask(..) )
import Control.Monad.Fail ( MonadFail(..) )
import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Accum ( AccumT )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Trans.Cont ( ContT )
import Control.Monad.Trans.Except ( ExceptT, mapExceptT )
import Control.Monad.Trans.Identity
    ( IdentityT, IdentityT(..), mapIdentityT )
import Control.Monad.Trans.Maybe ( MaybeT, mapMaybeT )
import Control.Monad.Trans.RWS ( RWST )
import Control.Monad.Trans.Reader
    ( ReaderT, ReaderT(..), mapReaderT )
import Control.Monad.Trans.Select
    ( SelectT, SelectT(SelectT), runSelectT )
import Control.Monad.Trans.State ( StateT )
import Control.Monad.Trans.State.Lazy ()
import Control.Monad.Trans.Writer ( WriterT )
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy.Internal ( defaultChunkSize )
import Data.Functor.Identity ()
import Data.IntMap ( IntMap )
import Data.Kind ( Type )
import Data.Monoid ()
import Data.Semigroup ()
import Data.Traversable ()
import Data.Void ( Void, absurd )
import Data.Word ()
import GHC.Exts ( RealWorld, State# )
import GHC.IO ( IO(..) )
import GHC.ST ( ST(..) )
import Prelude
    ( otherwise,
      ($),
      Bounded(minBound, maxBound),
      Eq((==)),
      Monad(..),
      Functor(fmap),
      Num((+), (-)),
      Show(show),
      Applicative((<*), pure, (<*>), (*>)),
      Semigroup(..),
      Monoid(mappend, mempty),
      Bool,
      String,
      Int,
      Maybe(..),
      type (~),
      Word,
      Either(..),
      const,
      ioError,
      (.),
      id,
      flip,
      concat,
      reverse,
      FilePath,
      either,
      maybe )
import qualified Control.Monad.Trans.Accum as Accum
    ( liftCallCC, liftCatch, liftListen, liftPass, mapAccumT )
import qualified Control.Monad.Trans.Writer.CPS as CPS
    ( listen, mapWriterT, pass, tell, writer, WriterT )
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
    ( ask,
      get,
      liftCallCC',
      liftCatch,
      listen,
      local,
      pass,
      put,
      reader,
      state,
      tell,
      writer,
      RWST )
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
    ( liftCallCC, liftCatch, WriterT )
import qualified Control.Monad.Trans.Cont as Cont ( liftLocal )
import qualified Control.Monad.Trans.Cont as ContT ( callCC )
import qualified Control.Exception as E
    ( try, catch, mask, mask_, throwIO, SomeException )
import qualified Control.Monad.Trans.Except as Except
    ( liftCallCC, liftListen, liftPass )
import qualified Control.Monad.Trans.Except as ExceptT
    ( catchE, throwE )
import qualified Data.IORef as I
    ( atomicModifyIORef, newIORef, IORef )
import qualified System.IO as IO
    ( hClose, IOMode(ReadMode), Handle, openBinaryFile )
import qualified Control.Monad.Trans.Identity as Identity
    ( liftCallCC, liftCatch, mapIdentityT )
import qualified Data.IntMap as IntMap
    ( delete, elems, empty, insert, lookup )
import qualified Control.Monad.ST.Lazy as L
    ( lazyToStrictST, strictToLazyST, ST )
import qualified Control.Monad.Trans.RWS.Lazy as Lazy ( RWST )
import qualified Control.Monad.Trans.State.Lazy as Lazy
    ( get, liftListen, liftPass, mapStateT, put, state, StateT )
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
    ( listen, mapWriterT, pass, tell, writer, WriterT )
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
    ( ask,
      get,
      liftCallCC',
      liftCatch,
      listen,
      local,
      pass,
      put,
      reader,
      state,
      tell,
      writer,
      RWST )
import qualified Control.Monad.Trans.State.Lazy as LazyState
    ( liftCallCC', liftCatch, StateT )
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
    ( liftCallCC, liftCatch, WriterT )
import qualified Control.Monad.Trans.Maybe as Maybe
    ( liftCallCC, liftCatch, liftListen, liftPass )
import qualified Control.Monad.Trans.Reader as Reader
    ( liftCallCC, liftCatch )
import qualified Control.Monad.Trans.Reader as ReaderT
    ( ask, local, reader )
import qualified Data.ByteString as S
    ( concat, drop, elemIndex, hGetSome, null, splitAt, ByteString )
import qualified Control.Monad.Trans.RWS.Strict as Strict ( RWST )
import qualified Control.Monad.Trans.State.Strict as Strict
    ( get, liftListen, liftPass, mapStateT, put, state, StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict
    ( listen, mapWriterT, pass, tell, writer, WriterT )
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
    ( ask,
      get,
      liftCallCC',
      liftCatch,
      listen,
      local,
      pass,
      put,
      reader,
      state,
      tell,
      writer,
      RWST )
import qualified Control.Monad.Trans.State.Strict as StrictState
    ( liftCallCC', liftCatch, StateT )
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
    ( liftCallCC, liftCatch, WriterT )

class Monad m => MonadCont (m :: Type -> Type) where
    {- | @callCC@ (call-with-current-continuation)
    calls a function with the current continuation as its argument.
    Provides an escape continuation mechanism for use with Continuation monads.
    Escape continuations allow to abort the current computation and return
    a value immediately.
    They achieve a similar effect to 'Control.Monad.Error.Class.throwError'
    and 'Control.Monad.Error.Class.catchError'
    within an 'Control.Monad.Except.Except' monad.
    Advantage of this function over calling @return@ is that it makes
    the continuation explicit,
    allowing more flexibility and better control
    (see examples in "Control.Monad.Cont").

    The standard idiom used with @callCC@ is to provide a lambda-expression
    to name the continuation. Then calling the named continuation anywhere
    within its scope will escape from the computation,
    even if it is many layers deep within nested computations.
    -}
    callCC :: ((a -> m b) -> m a) -> m a
    {-# MINIMAL callCC #-}

instance forall k (r :: k) (m :: (k -> Type)) . MonadCont (ContT r m) where
    callCC = ContT.callCC

instance MonadCont m => MonadCont (ExceptT e m) where
    callCC = Except.liftCallCC callCC

instance MonadCont m => MonadCont (IdentityT m) where
    callCC = Identity.liftCallCC callCC

instance MonadCont m => MonadCont (MaybeT m) where
    callCC = Maybe.liftCallCC callCC

instance MonadCont m => MonadCont (ReaderT r m) where
    callCC = Reader.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyRWS.RWST r w s m) where
    callCC = LazyRWS.liftCallCC' callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictRWS.RWST r w s m) where
    callCC = StrictRWS.liftCallCC' callCC

instance MonadCont m => MonadCont (LazyState.StateT s m) where
    callCC = LazyState.liftCallCC' callCC

instance MonadCont m => MonadCont (StrictState.StateT s m) where
    callCC = StrictState.liftCallCC' callCC

instance (Monoid w, MonadCont m) => MonadCont (LazyWriter.WriterT w m) where
    callCC = LazyWriter.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (StrictWriter.WriterT w m) where
    callCC = StrictWriter.liftCallCC callCC

instance (Monoid w, MonadCont m) => MonadCont (CPSRWS.RWST r w s m) where
    callCC = CPSRWS.liftCallCC' callCC

instance (Monoid w, MonadCont m) => MonadCont (CPSWriter.WriterT w m) where
    callCC = CPSWriter.liftCallCC callCC

instance
  ( Monoid w
  , MonadCont m
  ) => MonadCont (AccumT w m) where
    callCC = Accum.liftCallCC callCC

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own error type and\/or use a monad type constructor
other than @'Either' 'String'@ or @'Either' 'IOError'@.
In these cases you will have to explicitly define instances of the 'MonadError'
class.
(If you are using the deprecated "Control.Monad.Error" or
"Control.Monad.Trans.Error", you may also have to define an 'Error' instance.)
-}
class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a

    {- |
    A handler function to handle previous errors and return to normal execution.
    A common idiom is:

    > do { action1; action2; action3 } `catchError` handler

    where the @action@ functions can call 'throwError'.
    Note that @handler@ and the do-block must have the same return type.
    -}
    catchError :: m a -> (e -> m a) -> m a
    {-# MINIMAL throwError, catchError #-}

instance MonadError IOException IO where
    throwError = ioError
    catchError = E.catch

{- | @since 2.2.2 -}
instance MonadError () Maybe where
    throwError ()        = Nothing
    catchError Nothing f = f ()
    catchError x       _ = x

instance MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

{- | @since 2.2 -}
instance Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

instance MonadError e m => MonadError e (IdentityT m) where
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance MonadError e m => MonadError e (MaybeT m) where
    throwError = lift . throwError
    catchError = Maybe.liftCatch catchError

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (LazyRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = LazyRWS.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (StrictRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = StrictRWS.liftCatch catchError

instance MonadError e m => MonadError e (LazyState.StateT s m) where
    throwError = lift . throwError
    catchError = LazyState.liftCatch catchError

instance MonadError e m => MonadError e (StrictState.StateT s m) where
    throwError = lift . throwError
    catchError = StrictState.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (LazyWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = LazyWriter.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (StrictWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = StrictWriter.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (CPSRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = CPSRWS.liftCatch catchError

instance (Monoid w, MonadError e m) => MonadError e (CPSWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = CPSWriter.liftCatch catchError

instance
  ( Monoid w
  , MonadError e m
  ) => MonadError e (AccumT w m) where
    throwError = lift . throwError
    catchError = Accum.liftCatch catchError

class MonadIO m => MonadUnliftIO m where
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

instance MonadUnliftIO IO where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = inner id

instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runReaderT r)

instance MonadUnliftIO m => MonadUnliftIO (IdentityT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    IdentityT $
    withRunInIO $ \run ->
    inner (run . runIdentityT)

#if __GLASGOW_HASKELL__ < 802
type UnliftedType = TYPE 'PtrRepUnlifted
#elif __GLASGOW_HASKELL__ < 902
type UnliftedType = TYPE 'UnliftedRep
#endif

class Monad m => PrimMonad m where
  type PrimState m

  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

class PrimMonad m => PrimBase m where
  internal :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

instance PrimMonad IO where
  type PrimState IO = RealWorld
  primitive = IO
  {-# INLINE primitive #-}

instance PrimBase IO where
  internal (IO p) = p
  {-# INLINE internal #-}

instance PrimMonad m => PrimMonad (ContT r m) where
  type PrimState (ContT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (IdentityT m) where
  type PrimState (IdentityT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimBase m => PrimBase (IdentityT m) where
  internal (IdentityT m) = internal m
  {-# INLINE internal #-}

#if !MIN_VERSION_transformers(0,6,0)
instance PrimMonad m => PrimMonad (ListT m) where
  type PrimState (ListT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Error e, PrimMonad m) => PrimMonad (ErrorT e m) where
  type PrimState (ErrorT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance PrimMonad m => PrimMonad (MaybeT m) where
  type PrimState (MaybeT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (ReaderT r m) where
  type PrimState (ReaderT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (StateT s m) where
  type PrimState (StateT s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (WriterT w m) where
  type PrimState (WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,5,6)
instance (Monoid w, PrimMonad m) => PrimMonad (CPS.WriterT w m) where
  type PrimState (CPS.WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance (Monoid w, PrimMonad m) => PrimMonad (RWST r w s m) where
  type PrimState (RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,5,6)
instance (Monoid w, PrimMonad m) => PrimMonad (CPSRWS.RWST r w s m) where
  type PrimState (CPSRWS.RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance PrimMonad m => PrimMonad (ExceptT e m) where
  type PrimState (ExceptT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

#if MIN_VERSION_transformers(0,5,3)
instance ( Monoid w
         , PrimMonad m
         ) => PrimMonad (AccumT w m) where
  type PrimState (AccumT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (SelectT r m) where
  type PrimState (SelectT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
#endif

instance PrimMonad m => PrimMonad (Strict.StateT s m) where
  type PrimState (Strict.StateT s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (Strict.WriterT w m) where
  type PrimState (Strict.WriterT w m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (Monoid w, PrimMonad m) => PrimMonad (Strict.RWST r w s m) where
  type PrimState (Strict.RWST r w s m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

instance PrimBase (ST s) where
  internal (ST p) = p
  {-# INLINE internal #-}

#if __GLASGOW_HASKELL__ >= 802
instance PrimMonad (L.ST s) where
  type PrimState (L.ST s) = s
  primitive = L.strictToLazyST . primitive
  {-# INLINE primitive #-}

instance PrimBase (L.ST s) where
  internal = internal . L.lazyToStrictST
  {-# INLINE internal #-}
#endif

class (PrimMonad m, s ~ PrimState m) => MonadPrim s m
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

class (PrimBase m, MonadPrim s m) => MonadPrimBase s m
instance (PrimBase m, MonadPrim s m) => MonadPrimBase s m

class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m)
   => MonadRWS r w s m | m -> r, m -> w, m -> s

instance (Monoid w, Monad m) => MonadRWS r w s (CPSRWS.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Lazy.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Strict.RWST r w s m)

instance MonadRWS r w s m => MonadRWS r w s (ExceptT e m)
instance MonadRWS r w s m => MonadRWS r w s (IdentityT m)
instance MonadRWS r w s m => MonadRWS r w s (MaybeT m)

class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    ask   :: m r
    ask = reader id

    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = do
      r <- ask
      return (f r)

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT.ask
    local = ReaderT.local
    reader = ReaderT.reader

instance (Monad m, Monoid w) => MonadReader r (CPSRWS.RWST r w s m) where
    ask = CPSRWS.ask
    local = CPSRWS.local
    reader = CPSRWS.reader

instance (Monad m, Monoid w) => MonadReader r (LazyRWS.RWST r w s m) where
    ask = LazyRWS.ask
    local = LazyRWS.local
    reader = LazyRWS.reader

instance (Monad m, Monoid w) => MonadReader r (StrictRWS.RWST r w s m) where
    ask = StrictRWS.ask
    local = StrictRWS.local
    reader = StrictRWS.reader

instance MonadReader r' m => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local
    reader = lift . reader

{- | @since 2.2 -}
instance MonadReader r m => MonadReader r (ExceptT e m) where
    ask   = lift ask
    local = mapExceptT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (IdentityT m) where
    ask   = lift ask
    local = mapIdentityT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (MaybeT m) where
    ask   = lift ask
    local = mapMaybeT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
    ask   = lift ask
    local = Lazy.mapStateT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Strict.StateT s m) where
    ask   = lift ask
    local = Strict.mapStateT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (CPS.WriterT w m) where
    ask   = lift ask
    local = CPS.mapWriterT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (Lazy.WriterT w m) where
    ask   = lift ask
    local = Lazy.mapWriterT . local
    reader = lift . reader

instance (Monoid w, MonadReader r m) => MonadReader r (Strict.WriterT w m) where
    ask   = lift ask
    local = Strict.mapWriterT . local
    reader = lift . reader

instance
  ( Monoid w
  , MonadReader r m
  ) => MonadReader r (AccumT w m) where
    ask = lift ask
    local = Accum.mapAccumT . local
    reader = lift . reader

instance
  ( MonadReader r' m
  ) => MonadReader r' (SelectT r m) where
    ask = lift ask
    local f m = SelectT $ \c -> do
      r <- ask
      local f (runSelectT m (local (const r) . c))
    reader = lift . reader

class Monad m => MonadState s m | m -> s where
    get :: m s
    get = state (\s -> (s, s))

    put :: s -> m ()
    put s = state (\_ -> ((), s))

    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
    {-# MINIMAL state | get, put #-}

instance Monad m => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance Monad m => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put
    state = Strict.state

instance (Monad m, Monoid w) => MonadState s (CPSRWS.RWST r w s m) where
    get = CPSRWS.get
    put = CPSRWS.put
    state = CPSRWS.state

instance (Monad m, Monoid w) => MonadState s (LazyRWS.RWST r w s m) where
    get = LazyRWS.get
    put = LazyRWS.put
    state = LazyRWS.state

instance (Monad m, Monoid w) => MonadState s (StrictRWS.RWST r w s m) where
    get = StrictRWS.get
    put = StrictRWS.put
    state = StrictRWS.state

instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ExceptT e m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (IdentityT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (CPS.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Lazy.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (Strict.WriterT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance
  ( Monoid w
  , MonadState s m
  ) => MonadState s (AccumT w m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadState s m => MonadState s (SelectT r m) where
    get = lift get
    put = lift . put
    state = lift . state

#if !MIN_VERSION_transformers(0,6,0)
#endif

data ReleaseType
    = ReleaseEarly
    | ReleaseNormal
    | ReleaseExceptionWith E.SomeException
    deriving (Show)

class MonadIO m => MonadResource m where
    liftResourceT :: ResourceT IO a -> m a

data ReleaseKey = ReleaseKey !(I.IORef ReleaseMap) !Int

type RefCount = Word
type NextKey = Int

data ReleaseMap =
    ReleaseMap !NextKey !RefCount !(IntMap (ReleaseType -> IO ()))
  | ReleaseMapClosed

instance MonadCont m => MonadCont (ResourceT m) where
  callCC f = ResourceT $ \i -> callCC $ \c -> unResourceT (f (ResourceT . const . c)) i

instance MonadError e m => MonadError e (ResourceT m) where
  throwError = lift . throwError
  catchError r h = ResourceT $ \i -> unResourceT r i `catchError` \e -> unResourceT (h e) i

instance MonadRWS r w s m => MonadRWS r w s (ResourceT m)

instance MonadReader r m => MonadReader r (ResourceT m) where
  ask = lift ask
  local = mapResourceT . local

mapResourceT :: (m a -> n b) -> ResourceT m a -> ResourceT n b
mapResourceT f = ResourceT . (f .) . unResourceT

instance MonadState s m => MonadState s (ResourceT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (ResourceT m) where
  tell   = lift . tell
  listen = mapResourceT listen
  pass   = mapResourceT pass

instance MonadThrow m => MonadThrow (ResourceT m) where
    throwM = lift . throwM
instance MonadCatch m => MonadCatch (ResourceT m) where
  catch (ResourceT m) c =
      ResourceT $ \r -> m r `catch` \e -> unResourceT (c e) r
instance MonadMask m => MonadMask (ResourceT m) where
  mask a = ResourceT $ \e -> mask $ \u -> unResourceT (a $ q u) e
    where q u (ResourceT b) = ResourceT (u . b)
  uninterruptibleMask a =
    ResourceT $ \e -> uninterruptibleMask $ \u -> unResourceT (a $ q u) e
      where q u (ResourceT b) = ResourceT (u . b)
#if MIN_VERSION_exceptions(0, 10, 0)
  generalBracket acquire cleanup use =
    ResourceT $ \r ->
        generalBracket
            ( unResourceT acquire r )
            ( \resource exitCase ->
                  unResourceT ( cleanup resource exitCase ) r
            )
            ( \resource -> unResourceT ( use resource ) r )
#elif MIN_VERSION_exceptions(0, 9, 0)
#error exceptions 0.9.0 is not supported
#endif
instance MonadIO m => MonadResource (ResourceT m) where
    liftResourceT = transResourceT liftIO
instance PrimMonad m => PrimMonad (ResourceT m) where
    type PrimState (ResourceT m) = PrimState m
    primitive = lift . primitive

transResourceT :: (m a -> n b)
               -> ResourceT m a
               -> ResourceT n b
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))

newtype ResourceT m a = ResourceT { unResourceT :: I.IORef ReleaseMap -> m a }

data InvalidAccess = InvalidAccess String

instance Show InvalidAccess where
    show (InvalidAccess f) = concat
        [ "Control.Monad.Trans.Resource."
        , f
        , ": The mutable state is being accessed after cleanup. Please contact the maintainers."
        ]

instance Exception InvalidAccess

instance Functor m => Functor (ResourceT m) where
    fmap f (ResourceT m) = ResourceT $ \r -> fmap f (m r)

instance Applicative m => Applicative (ResourceT m) where
    pure = ResourceT . const . pure
    ResourceT mf <*> ResourceT ma = ResourceT $ \r ->
        mf r <*> ma r
    ResourceT mf *> ResourceT ma = ResourceT $ \r ->
        mf r *> ma r
    ResourceT mf <* ResourceT ma = ResourceT $ \r ->
        mf r <* ma r

instance Alternative m => Alternative (ResourceT m) where
    empty = ResourceT $ \_ -> empty
    (ResourceT mf) <|> (ResourceT ma) = ResourceT $ \r -> mf r <|> ma r

instance MonadPlus m => MonadPlus (ResourceT m) where
    mzero = ResourceT $ \_ -> mzero
    (ResourceT mf) `mplus` (ResourceT ma) = ResourceT $ \r -> mf r `mplus` ma r

instance Monad m => Monad (ResourceT m) where
    return = pure
    ResourceT ma >>= f = ResourceT $ \r -> do
        a <- ma r
        let ResourceT f' = f a
        f' r

instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . Control.Monad.Fail.fail

instance MonadFix m => MonadFix (ResourceT m) where
  mfix f = ResourceT $ \r -> mfix $ \a -> unResourceT (f a) r

instance MonadTrans ResourceT where
    lift = ResourceT . const

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO

instance MonadUnliftIO m => MonadUnliftIO (ResourceT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    ResourceT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip unResourceT r)

#define GO(T) instance (MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
#define GOX(X, T) instance (X, MonadResource m) => MonadResource (T m) where liftResourceT = lift . liftResourceT
GO(IdentityT)
#if !MIN_VERSION_transformers(0,6,0)
GO(ListT)
#endif
GO(MaybeT)
GO(ExceptT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

register' :: I.IORef ReleaseMap
          -> IO ()
          -> IO ReleaseKey
register' istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key (const rel) m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

data ResourceCleanupException = ResourceCleanupException
  { rceOriginalException :: !(Maybe SomeException)
  , rceFirstCleanupException :: !SomeException
  , rceOtherCleanupExceptions :: ![SomeException]
  }
  deriving (Show)
instance Exception ResourceCleanupException

stateCleanupChecked
  :: Maybe SomeException -- ^ exception that killed the 'ResourceT', if present
  -> I.IORef ReleaseMap -> IO ()
stateCleanupChecked morig istate = E.mask_ $ do
    mm <- I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                let rf' = rf - 1
                 in if rf' == minBound
                        then (ReleaseMapClosed, Just m)
                        else (ReleaseMap nk rf' m, Nothing)
            ReleaseMapClosed -> throw $ InvalidAccess "stateCleanupChecked"
    case mm of
        Just m -> do
            res <- mapMaybeReverseM (\x -> try (x rtype)) $ IntMap.elems m
            case res of
                [] -> return () -- nothing went wrong
                e:es -> E.throwIO $ ResourceCleanupException morig e es
        Nothing -> return ()
  where
    try :: IO () -> IO (Maybe SomeException)
    try io = fmap (either Just (\() -> Nothing)) (E.try io)

    rtype = maybe ReleaseNormal ReleaseExceptionWith morig

mapMaybeReverseM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeReverseM f =
    go []
  where
    go bs [] = return bs
    go bs (a:as) = do
      mb <- f a
      case mb of
        Nothing -> go bs as
        Just b -> go (b:bs) as

release :: MonadIO m => ReleaseKey -> m ()
release (ReleaseKey istate rk) = liftIO $ release' istate rk
    (maybe (return ()) id)

allocate :: MonadResource m
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free resource
         -> m (ReleaseKey, a)
allocate a = liftResourceT . allocateRIO a

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask_ $ do
    a <- acquire
    key <- register' istate $ rel a
    return (key, a)

release' :: I.IORef ReleaseMap
         -> Int
         -> (Maybe (IO ()) -> IO a)
         -> IO a
release' istate key act = E.mask_ $ do
    maction <- I.atomicModifyIORef istate lookupAction
    act maction
  where
    lookupAction rm@(ReleaseMap next rf m) =
        case IntMap.lookup key m of
            Nothing -> (rm, Nothing)
            Just action ->
                ( ReleaseMap next rf $ IntMap.delete key m
                , Just (action ReleaseEarly)
                )
    lookupAction ReleaseMapClosed = (ReleaseMapClosed, Nothing)

runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT (ResourceT r) = withRunInIO $ \run -> do
    istate <- createInternalState
    E.mask $ \restore -> do
        res <- restore (run (r istate)) `E.catch` \e -> do
            stateCleanupChecked (Just e) istate
            E.throwIO e
        stateCleanupChecked Nothing istate
        return res

createInternalState :: MonadIO m => m InternalState
createInternalState = liftIO
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty

type InternalState = I.IORef ReleaseMap

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    {-# MINIMAL (writer | tell), listen, pass #-}
    writer :: (a,w) -> m a
    writer ~(a, w) = do
      tell w
      return a

    tell   :: w -> m ()
    tell w = writer ((),w)

    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a

censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

instance (Monoid w) => MonadWriter w ((,) w) where
  writer ~(a, w) = (w, a)
  tell w = (w, ())
  listen ~(w, a) = (w, (a, w))
  pass ~(w, (a, f)) = (f w, a)

instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
    writer = CPS.writer
    tell   = CPS.tell
    listen = CPS.listen
    pass   = CPS.pass

instance (Monoid w, Monad m) => MonadWriter w (Lazy.WriterT w m) where
    writer = Lazy.writer
    tell   = Lazy.tell
    listen = Lazy.listen
    pass   = Lazy.pass

instance (Monoid w, Monad m) => MonadWriter w (Strict.WriterT w m) where
    writer = Strict.writer
    tell   = Strict.tell
    listen = Strict.listen
    pass   = Strict.pass

instance (Monoid w, Monad m) => MonadWriter w (CPSRWS.RWST r w s m) where
    writer = CPSRWS.writer
    tell   = CPSRWS.tell
    listen = CPSRWS.listen
    pass   = CPSRWS.pass

instance (Monoid w, Monad m) => MonadWriter w (LazyRWS.RWST r w s m) where
    writer = LazyRWS.writer
    tell   = LazyRWS.tell
    listen = LazyRWS.listen
    pass   = LazyRWS.pass

instance (Monoid w, Monad m) => MonadWriter w (StrictRWS.RWST r w s m) where
    writer = StrictRWS.writer
    tell   = StrictRWS.tell
    listen = StrictRWS.listen
    pass   = StrictRWS.pass

instance MonadWriter w m => MonadWriter w (ExceptT e m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Except.liftListen listen
    pass   = Except.liftPass pass

instance MonadWriter w m => MonadWriter w (IdentityT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Identity.mapIdentityT listen
    pass   = Identity.mapIdentityT pass

instance MonadWriter w m => MonadWriter w (MaybeT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Maybe.liftListen listen
    pass   = Maybe.liftPass pass

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = mapReaderT listen
    pass   = mapReaderT pass

instance MonadWriter w m => MonadWriter w (Lazy.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Lazy.liftListen listen
    pass   = Lazy.liftPass pass

instance MonadWriter w m => MonadWriter w (Strict.StateT s m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Strict.liftListen listen
    pass   = Strict.liftPass pass

instance
  ( Monoid w'
  , MonadWriter w m
  ) => MonadWriter w (AccumT w' m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Accum.liftListen listen
    pass   = Accum.liftPass pass

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x, y, z) 0
#endif

data Pipe l i o u m r =
    HaveOutput (Pipe l i o u m r) o
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
  | Done r
  | PipeM (m (Pipe l i o u m r))
  | Leftover (Pipe l i o u m r) l

instance Monad m => Functor (Pipe l i o u m) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Monad m => Applicative (Pipe l i o u m) where
    pure = Done
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad m => Monad (Pipe l i o u m) where
    return = pure
    {-# INLINE return #-}

    HaveOutput p o >>= fp = HaveOutput (p >>= fp) o
    NeedInput p c >>= fp = NeedInput (\i -> p i >>= fp) (\u -> c u >>= fp)
    Done x >>= fp = fp x
    PipeM mp >>= fp = PipeM ((>>= fp) `liftM` mp)
    Leftover p i >>= fp = Leftover (p >>= fp) i

instance MonadTrans (Pipe l i o u) where
    lift mr = PipeM (Done `liftM` mr)
    {-# INLINE [1] lift #-}

instance MonadIO m => MonadIO (Pipe l i o u m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadThrow m => MonadThrow (Pipe l i o u m) where
    throwM = lift . throwM
    {-# INLINE throwM #-}

instance Monad m => Semigroup (Pipe l i o u m ()) where
    (<>) = (>>)
    {-# INLINE (<>) #-}

instance Monad m => Monoid (Pipe l i o u m ()) where
    mempty = return ()
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
    {-# INLINE mappend #-}
#endif

instance PrimMonad m => PrimMonad (Pipe l i o u m) where
  type PrimState (Pipe l i o u m) = PrimState m
  primitive = lift . primitive

instance MonadResource m => MonadResource (Pipe l i o u m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

instance MonadReader r m => MonadReader r (Pipe l i o u m) where
    ask = lift ask
    {-# INLINE ask #-}
    local f (HaveOutput p o) = HaveOutput (local f p) o
    local f (NeedInput p c) = NeedInput (\i -> local f (p i)) (\u -> local f (c u))
    local _ (Done x) = Done x
    local f (PipeM mp) = PipeM (liftM (local f) $ local f mp)
    local f (Leftover p i) = Leftover (local f p) i

instance MonadWriter w m => MonadWriter w (Pipe l i o u m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif
    tell = lift . tell

    listen (HaveOutput p o) = HaveOutput (listen p) o
    listen (NeedInput p c) = NeedInput (\i -> listen (p i)) (\u -> listen (c u))
    listen (Done x) = Done (x, mempty)
    listen (PipeM mp) = PipeM $ do
        (p, w) <- listen mp
        return $ do
            (x, w') <- listen p
            return (x, w `mappend` w')
    listen (Leftover p i) = Leftover (listen p) i

    pass (HaveOutput p o) = HaveOutput (pass p) o
    pass (NeedInput p c) = NeedInput (\i -> pass (p i)) (\u -> pass (c u))
    pass (PipeM mp) = PipeM $ mp >>= (return . pass)
    pass (Done (x, _)) = Done x
    pass (Leftover p i) = Leftover (pass p) i

instance MonadState s m => MonadState s (Pipe l i o u m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (Pipe l i o u m)

instance MonadError e m => MonadError e (Pipe l i o u m) where
    throwError = lift . throwError
    catchError (HaveOutput p o) f = HaveOutput (catchError p f) o
    catchError (NeedInput p c) f = NeedInput (\i -> catchError (p i) f) (\u -> catchError (c u) f)
    catchError (Done x) _ = Done x
    catchError (PipeM mp) f = PipeM $ catchError (liftM (flip catchError f) mp) (\e -> return (f e))
    catchError (Leftover p i) f = Leftover (catchError p f) i

runPipe :: Monad m => Pipe Void () Void () m r -> m r
runPipe (HaveOutput _ o) = absurd o
runPipe (NeedInput _ c) = runPipe (c ())
runPipe (Done r) = return r
runPipe (PipeM mp) = mp >>= runPipe
runPipe (Leftover _ i) = absurd i

injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
injectLeftovers = go []
  where
    go ls (HaveOutput p o) = HaveOutput (go ls p) o
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)
    go _ (Done r) = Done r
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p

newtype ConduitT i o m r = ConduitT
    { unConduitT :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

instance Functor (ConduitT i o m) where
    fmap f (ConduitT c) = ConduitT $ \rest -> c (rest . f)

instance Applicative (ConduitT i o m) where
    pure x = ConduitT ($ x)
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}
    x *> y = x >>= \_ -> y
    {-# INLINE (*>) #-}

instance Monad (ConduitT i o m) where
    return = pure
    ConduitT f >>= g = ConduitT $ \h -> f $ \a -> unConduitT (g a) h

instance MonadFail m => MonadFail (ConduitT i o m) where
    fail = lift . Control.Monad.Fail.fail

instance MonadThrow m => MonadThrow (ConduitT i o m) where
    throwM = lift . throwM

instance MonadIO m => MonadIO (ConduitT i o m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance MonadReader r m => MonadReader r (ConduitT i o m) where
    ask = lift ask
    {-# INLINE ask #-}

    local f (ConduitT c0) = ConduitT $ \rest ->
        let go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput p c) = NeedInput (\i -> go (p i)) (\u -> go (c u))
            go (Done x) = rest x
            go (PipeM mp) = PipeM (liftM go $ local f mp)
            go (Leftover p i) = Leftover (go p) i
         in go (c0 Done)

instance MonadWriter w m => MonadWriter w (ConduitT i o m) where
#if MIN_VERSION_mtl(2, 1, 0)
    writer = lift . writer
#endif
    tell = lift . tell

    listen (ConduitT c0) = ConduitT $ \rest ->
        let go front (HaveOutput p o) = HaveOutput (go front p) o
            go front (NeedInput p c) = NeedInput (\i -> go front (p i)) (\u -> go front (c u))
            go front (Done x) = rest (x, front)
            go front (PipeM mp) = PipeM $ do
                (p,w) <- listen mp
                return $ go (front `mappend` w) p
            go front (Leftover p i) = Leftover (go front p) i
         in go mempty (c0 Done)

    pass (ConduitT c0) = ConduitT $ \rest ->
        let go front (HaveOutput p o) = HaveOutput (go front p) o
            go front (NeedInput p c) = NeedInput (\i -> go front (p i)) (\u -> go front (c u))
            go front (PipeM mp) = PipeM $ do
                (p,w) <- censor (const mempty) (listen mp)
                return $ go (front `mappend` w) p
            go front (Done (x,f)) = PipeM $ do
                tell (f front)
                return $ rest x
            go front (Leftover p i) = Leftover (go front p) i
         in go mempty (c0 Done)

instance MonadState s m => MonadState s (ConduitT i o m) where
    get = lift get
    put = lift . put
#if MIN_VERSION_mtl(2, 1, 0)
    state = lift . state
#endif

instance MonadRWS r w s m => MonadRWS r w s (ConduitT i o m)

instance MonadError e m => MonadError e (ConduitT i o m) where
    throwError = lift . throwError
    catchError (ConduitT c0) f = ConduitT $ \rest ->
        let go (HaveOutput p o) = HaveOutput (go p) o
            go (NeedInput p c) = NeedInput (\i -> go (p i)) (\u -> go (c u))
            go (Done x) = rest x
            go (PipeM mp) =
              PipeM $ catchError (liftM go mp) $ \e -> do
                return $ unConduitT (f e) rest
            go (Leftover p i) = Leftover (go p) i
         in go (c0 Done)

instance MonadTrans (ConduitT i o) where
    lift mr = ConduitT $ \rest -> PipeM (liftM rest mr)
    {-# INLINE [1] lift #-}

instance MonadResource m => MonadResource (ConduitT i o m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

instance Monad m => Semigroup (ConduitT i o m ()) where
    (<>) = (>>)
    {-# INLINE (<>) #-}

instance Monad m => Monoid (ConduitT i o m ()) where
    mempty = return ()
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
    {-# INLINE mappend #-}
#endif

instance PrimMonad m => PrimMonad (ConduitT i o m) where
  type PrimState (ConduitT i o m) = PrimState m
  primitive = lift . primitive

infixr 2 .|
(.|) :: Monad m
     => ConduitT a b m () -- ^ upstream
     -> ConduitT b c m r -- ^ downstream
     -> ConduitT a c m r
ConduitT left0 .| ConduitT right0 = ConduitT $ \rest ->
    let goRight left right =
            case right of
                HaveOutput p o    -> HaveOutput (recurse p) o
                NeedInput rp rc   -> goLeft rp rc left
                Done r2           -> rest r2
                PipeM mp          -> PipeM (liftM recurse mp)
                Leftover right' i -> goRight (HaveOutput left i) right'
          where
            recurse = goRight left

        goLeft rp rc left =
            case left of
                HaveOutput left' o        -> goRight left' (rp o)
                NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
                Done r1                   -> goRight (Done r1) (rc r1)
                PipeM mp                  -> PipeM (liftM recurse mp)
                Leftover left' i          -> Leftover (recurse left') i
          where
            recurse = goLeft rp rc
     in goRight (left0 Done) (right0 Done)
{-# INLINE (.|) #-}

await :: Monad m => ConduitT i o m (Maybe i)
await = ConduitT $ \f -> NeedInput (f . Just) (const $ f Nothing)
{-# INLINE [0] await #-}

yield :: Monad m
      => o -- ^ output value
      -> ConduitT i o m ()
yield o = ConduitT $ \rest -> HaveOutput (rest ()) o
{-# INLINE yield #-}

leftover :: i -> ConduitT i o m ()
leftover i = ConduitT $ \rest -> Leftover (rest ()) i
{-# INLINE leftover #-}

runConduit :: Monad m => ConduitT () Void m r -> m r
runConduit (ConduitT p) = runPipe $ injectLeftovers $ p Done
{-# INLINE [0] runConduit #-}

bracketP :: MonadResource m

         => IO a
         -> (a -> IO ())
         -> (a -> ConduitT i o m r)
         -> ConduitT i o m r
bracketP alloc free inside = ConduitT $ \rest -> do
  (key, seed) <- allocate alloc free
  unConduitT (inside seed) $ \res -> do
    release key
    rest res

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

sourceFile :: MonadResource m => FilePath -> ConduitT i ByteString m ()
sourceFile fp =
  bracketP
    (IO.openBinaryFile fp IO.ReadMode)
    IO.hClose
    sourceHandle

sourceHandle :: MonadIO m => IO.Handle -> ConduitT i ByteString m ()
sourceHandle h = loop
  where
    loop = do
      bs <- liftIO $ S.hGetSome h defaultChunkSize
      if S.null bs
        then return ()
        else yield bs >> loop

takeWhile :: Monad m => (a -> Bool) -> ConduitT a a m ()
takeWhile f = loop
  where
    loop = await >>= maybe (return ()) go
    go x
      | f x = yield x >> loop
      | otherwise = leftover x
