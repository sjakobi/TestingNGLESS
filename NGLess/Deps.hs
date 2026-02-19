{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT)
import qualified Control.Monad.Trans.Cont as ContT
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as ExceptT (catchE, runExceptT, throwE)
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import Control.Monad.Trans.Class (lift)
import Control.Exception (IOException, ioError)
import Control.Monad (Monad)
import Data.Monoid (Monoid)
import Prelude (Either (Left, Right), Maybe (Nothing), either, flip, (.), IO, pure, (<$>), (>>=))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Data.Kind (Type)
import GHC.Exts   ( State#, RealWorld, noDuplicate#, touch#
                  , unsafeCoerce#, realWorld#, seq#
                  , TYPE
#if __GLASGOW_HASKELL__ >= 902
                  , UnliftedType
#endif
#if defined(HAVE_KEEPALIVE)
                  , keepAliveLiftedLifted#
                  , keepAliveUnliftedLifted#
#endif
                  )
import GHC.IO     ( IO(..) )
import GHC.ST     ( ST(..) )
import qualified Control.Monad.ST.Lazy as L
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont     ( ContT    )
import Control.Monad.Trans.Identity ( IdentityT (IdentityT) )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Error    ( ErrorT, Error)
#endif
import Control.Monad.Trans.Except   ( ExceptT  )
import Control.Monad.Trans.Accum    ( AccumT   )
import Control.Monad.Trans.Select   ( SelectT  )
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import Control.Monad.Trans.Select (SelectT (SelectT), runSelectT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT) 
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
        (State, runState, evalState, execState, mapState, withState,
         StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Exception (throw, Exception, SomeException)
import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Identity (IdentityT)
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List (ListT)
#endif
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Catch (MonadThrow (..), MonadCatch (..), MonadMask (..))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IORef as I
import Data.Typeable
import Data.Word (Word)
import qualified Control.Exception as E
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS 
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS 
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict 
import Control.Monad.Trans.Accum (AccumT)
import qualified Control.Monad.Trans.Accum as Accum
import qualified Control.Monad.Trans.RWS.CPS as CPSRWS
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Applicative (..))
import Control.Exception (Exception)
import qualified Control.Exception as E (catch)
import Control.Monad (liftM, liftM2, ap)
import Control.Monad.Fail(MonadFail(..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Void (Void, absurd)
import Data.Monoid (Monoid (mappend, mempty))
import Data.Semigroup (Semigroup ((<>)))
import Control.Monad (forever)
import Data.Traversable (Traversable (..))
import           Control.Monad (unless)
import qualified Data.ByteString as S
import           Prelude hiding (lines, takeWhile)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Prelude hiding (lines, takeWhile)
import qualified System.IO as IO

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

newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

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

askUnliftIO :: MonadUnliftIO m => m (UnliftIO m)
askUnliftIO = withRunInIO (\run -> return (UnliftIO run))
{-# INLINE askUnliftIO #-}

{-# INLINE askRunInIO #-}
askRunInIO :: MonadUnliftIO m => m (m a -> IO a)
askRunInIO = withRunInIO (\run -> (return (\ma -> run ma)))

{-# INLINE withUnliftIO #-}
withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
withUnliftIO inner = askUnliftIO >>= liftIO . inner

{-# INLINE toIO #-}
toIO :: MonadUnliftIO m => m a -> m (IO a)
toIO m = withRunInIO $ \run -> return $ run m

{- | A helper function for implementing @MonadUnliftIO@ instances.
Useful for the common case where you want to simply delegate to the
underlying transformer.

Note: You can derive 'MonadUnliftIO' for newtypes without this helper function
in @unliftio-core@ 0.2.0.0 and later.

@since 0.1.2.0
==== __Example__

> newtype AppT m a = AppT { unAppT :: ReaderT Int (ResourceT m) a }
>   deriving (Functor, Applicative, Monad, MonadIO)
>
> -- Same as `deriving newtype (MonadUnliftIO)`
> instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
>   withRunInIO = wrappedWithRunInIO AppT unAppT
-}
{-# INLINE wrappedWithRunInIO #-}
wrappedWithRunInIO :: MonadUnliftIO n
                   => (n b -> m b)
                   -> (forall a. m a -> n a)
                   -> ((forall a. m a -> IO a) -> IO b)
                   -> m b
wrappedWithRunInIO wrap unwrap inner = wrap $ withRunInIO $ \run ->
  inner $ run . unwrap

{- | A helper function for lifting @IO a -> IO b@ functions into any @MonadUnliftIO@.

=== __Example__

> liftedTry :: (Exception e, MonadUnliftIO m) => m a -> m (Either e a)
> liftedTry m = liftIOOp Control.Exception.try m

@since 0.2.1.0
-}
liftIOOp :: MonadUnliftIO m => (IO a -> IO b) -> m a -> m b
liftIOOp f x = do
  runInIO <- askRunInIO
  liftIO $ f $ runInIO x

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

primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# ->
    case f s# of
        s'# -> (# s'#, () #))

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

liftPrim
  :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2) => m1 a -> m2 a
{-# INLINE liftPrim #-}
liftPrim = primToPrim

primToPrim :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2)
        => m1 a -> m2 a
{-# INLINE primToPrim #-}
primToPrim m = primitive (internal m)

primToIO :: (PrimBase m, PrimState m ~ RealWorld) => m a -> IO a
{-# INLINE primToIO #-}
primToIO = primToPrim

primToST :: PrimBase m => m a -> ST (PrimState m) a
{-# INLINE primToST #-}
primToST = primToPrim

ioToPrim :: (PrimMonad m, PrimState m ~ RealWorld) => IO a -> m a
{-# INLINE ioToPrim #-}
ioToPrim = primToPrim

stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
{-# INLINE stToPrim #-}
stToPrim = primToPrim

unsafePrimToPrim :: (PrimBase m1, PrimMonad m2) => m1 a -> m2 a
{-# INLINE unsafePrimToPrim #-}
unsafePrimToPrim m = primitive (unsafeCoerce# (internal m))

unsafePrimToST :: PrimBase m => m a -> ST s a
{-# INLINE unsafePrimToST #-}
unsafePrimToST = unsafePrimToPrim

unsafePrimToIO :: PrimBase m => m a -> IO a
{-# INLINE unsafePrimToIO #-}
unsafePrimToIO = unsafePrimToPrim

unsafeSTToPrim :: PrimMonad m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

unsafeIOToPrim :: PrimMonad m => IO a -> m a
{-# INLINE unsafeIOToPrim #-}
unsafeIOToPrim = unsafePrimToPrim

unsafeInlinePrim :: PrimBase m => m a -> a
{-# INLINE unsafeInlinePrim #-}
unsafeInlinePrim m = unsafeInlineIO (unsafePrimToIO m)

unsafeInlineIO :: IO a -> a
{-# INLINE unsafeInlineIO #-}
unsafeInlineIO m = case internal m realWorld# of (# _, r #) -> r

unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

touch :: PrimMonad m => a -> m ()
{-# INLINE touch #-}
touch x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

touchUnlifted :: forall (m :: Type -> Type) (a :: UnliftedType). PrimMonad m => a -> m ()
{-# INLINE touchUnlifted #-}
touchUnlifted x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

keepAlive :: PrimBase m
  => a -- ^ Value @x@ to keep alive while computation @k@ runs.
  -> m r -- ^ Computation @k@
  -> m r
#if defined(HAVE_KEEPALIVE)
{-# INLINE keepAlive #-}
keepAlive x k =
  primitive $ \s0 -> keepAliveLiftedLifted# x s0 (internal k)

#else
{-# NOINLINE keepAlive #-}
keepAlive x k = k <* touch x
#endif

keepAliveUnlifted :: forall (m :: Type -> Type) (a :: UnliftedType) (r :: Type). PrimBase m => a -> m r -> m r
#if defined(HAVE_KEEPALIVE)
{-# INLINE keepAliveUnlifted #-}
keepAliveUnlifted x k =
  primitive $ \s0 -> keepAliveUnliftedLifted# x s0 (internal k)

#else
{-# NOINLINE keepAliveUnlifted #-}
keepAliveUnlifted x k = k <* touchUnlifted x
#endif

evalPrim :: forall a m . PrimMonad m => a -> m a
evalPrim a = primitive (\s -> seq# a s)

noDuplicate :: PrimMonad m => m ()
#if __GLASGOW_HASKELL__ >= 802
noDuplicate = primitive $ \ s -> (# noDuplicate# s, () #)
#else
noDuplicate = unsafeIOToPrim $ primitive $ \s -> (# noDuplicate# s, () #)
#endif

unsafeInterleave, unsafeDupableInterleave :: PrimBase m => m a -> m a
unsafeInterleave x = unsafeDupableInterleave (noDuplicate >> x)
unsafeDupableInterleave x = primitive $ \ s -> let r' = case internal x s of (# _, r #) -> r in (# s, r' #)
{-# INLINE unsafeInterleave #-}
{-# NOINLINE unsafeDupableInterleave #-}

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

modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

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
    deriving (Show, Typeable)

class MonadIO m => MonadResource m where
    liftResourceT :: ResourceT IO a -> m a

data ReleaseKey = ReleaseKey !(I.IORef ReleaseMap) !Int
    deriving Typeable

type RefCount = Word
type NextKey = Int

data ReleaseMap =
    ReleaseMap !NextKey !RefCount !(IntMap (ReleaseType -> IO ()))
  | ReleaseMapClosed

type ResIO = ResourceT IO

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
  generalBracket acquire release use =
    ResourceT $ \r ->
        generalBracket
            ( unResourceT acquire r )
            ( \resource exitCase ->
                  unResourceT ( release resource exitCase ) r
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
#if __GLASGOW_HASKELL__ >= 707
        deriving Typeable
#else
instance Typeable1 m => Typeable1 (ResourceT m) where
    typeOf1 = goType undefined
      where
        goType :: Typeable1 m => m a -> ResourceT m a -> TypeRep
        goType m _ =
            mkTyConApp
#if __GLASGOW_HASKELL__ >= 704
                (mkTyCon3 "resourcet" "Control.Monad.Trans.Resource" "ResourceT")
#else
                (mkTyCon "Control.Monad.Trans.Resource.ResourceT")
#endif
                [ typeOf1 m
                ]
#endif

data InvalidAccess = InvalidAccess { functionName :: String }
    deriving Typeable

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

stateAlloc :: I.IORef ReleaseMap -> IO ()
stateAlloc istate = do
    I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                (ReleaseMap nk (rf + 1) m, ())
            ReleaseMapClosed -> throw $ InvalidAccess "stateAlloc"

stateCleanup :: ReleaseType -> I.IORef ReleaseMap -> IO ()
stateCleanup rtype istate = E.mask_ $ do
    mm <- I.atomicModifyIORef istate $ \rm ->
        case rm of
            ReleaseMap nk rf m ->
                let rf' = rf - 1
                 in if rf' == minBound
                        then (ReleaseMapClosed, Just m)
                        else (ReleaseMap nk rf' m, Nothing)
            ReleaseMapClosed -> throw $ InvalidAccess "stateCleanup"
    case mm of
        Just m ->
            mapM_ (\x -> try (x rtype) >> return ()) $ IntMap.elems m
        Nothing -> return ()
  where
    try :: IO a -> IO (Either SomeException a)
    try = E.try

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

registerType :: I.IORef ReleaseMap
             -> (ReleaseType -> IO ())
             -> IO ReleaseKey
registerType istate rel = I.atomicModifyIORef istate $ \rm ->
    case rm of
        ReleaseMap key rf m ->
            ( ReleaseMap (key - 1) rf (IntMap.insert key rel m)
            , ReleaseKey istate key
            )
        ReleaseMapClosed -> throw $ InvalidAccess "register'"

data ResourceCleanupException = ResourceCleanupException
  { rceOriginalException :: !(Maybe SomeException)
  , rceFirstCleanupException :: !SomeException
  , rceOtherCleanupExceptions :: ![SomeException]
  }
  deriving (Show, Typeable)
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

register :: MonadResource m => IO () -> m ReleaseKey
register = liftResourceT . registerRIO

release :: MonadIO m => ReleaseKey -> m ()
release (ReleaseKey istate rk) = liftIO $ release' istate rk
    (maybe (return ()) id)

unprotect :: MonadIO m => ReleaseKey -> m (Maybe (IO ()))
unprotect (ReleaseKey istate rk) = liftIO $ release' istate rk return

allocate :: MonadResource m
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free resource
         -> m (ReleaseKey, a)
allocate a = liftResourceT . allocateRIO a

allocate_ :: MonadResource m
          => IO a -- ^ allocate
          -> IO () -- ^ free resource
          -> m ReleaseKey
allocate_ a = fmap fst . allocate a . const

resourceMask :: MonadResource m => ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> m b
resourceMask r = liftResourceT (resourceMaskRIO r)

allocateRIO :: IO a -> (a -> IO ()) -> ResourceT IO (ReleaseKey, a)
allocateRIO acquire rel = ResourceT $ \istate -> liftIO $ E.mask_ $ do
    a <- acquire
    key <- register' istate $ rel a
    return (key, a)

registerRIO :: IO () -> ResourceT IO ReleaseKey
registerRIO rel = ResourceT $ \istate -> liftIO $ register' istate rel

resourceMaskRIO :: ((forall a. ResourceT IO a -> ResourceT IO a) -> ResourceT IO b) -> ResourceT IO b
resourceMaskRIO f = ResourceT $ \istate -> liftIO $ E.mask $ \restore ->
    let ResourceT f' = f (go restore)
     in f' istate
  where
    go :: (forall a. IO a -> IO a) -> (forall a. ResourceT IO a -> ResourceT IO a)
    go r (ResourceT g) = ResourceT (\i -> r (g i))

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

runResourceTChecked :: MonadUnliftIO m => ResourceT m a -> m a
runResourceTChecked = runResourceT
{-# INLINE runResourceTChecked #-}

bracket_ :: MonadUnliftIO m
         => IO () -- ^ allocate
         -> IO () -- ^ normal cleanup
         -> (E.SomeException -> IO ()) -- ^ exceptional cleanup
         -> m a
         -> m a
bracket_ alloc cleanupNormal cleanupExc inside =
    withRunInIO $ \run -> E.mask $ \restore -> do
        alloc
        res <- restore (run inside) `E.catch` (\e -> cleanupExc e >> E.throwIO e)
        cleanupNormal
        return res

joinResourceT :: ResourceT (ResourceT m) a
              -> ResourceT m a
joinResourceT (ResourceT f) = ResourceT $ \r -> unResourceT (f r) r

resourceForkWith
  :: MonadUnliftIO m
  => (IO () -> IO a)
  -> ResourceT m ()
  -> ResourceT m a
resourceForkWith g (ResourceT f) =
  ResourceT $ \r -> withRunInIO $ \run -> E.mask $ \restore ->
    bracket_
        (stateAlloc r)
        (return ())
        (const $ return ())
        (g $ bracket_
            (return ())
            (stateCleanup ReleaseNormal r)
            (\e -> stateCleanup (ReleaseExceptionWith e) r)
            (restore $ run $ f r))

resourceForkIO :: MonadUnliftIO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO = resourceForkWith forkIO

type MonadResourceBase = MonadUnliftIO
{-# DEPRECATED MonadResourceBase "Use MonadUnliftIO directly instead" #-}

createInternalState :: MonadIO m => m InternalState
createInternalState = liftIO
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty

closeInternalState :: MonadIO m => InternalState -> m ()
closeInternalState = liftIO . stateCleanup ReleaseNormal

getInternalState :: Monad m => ResourceT m InternalState
getInternalState = ResourceT return

type InternalState = I.IORef ReleaseMap

runInternalState :: ResourceT m a -> InternalState -> m a
runInternalState = unResourceT

withInternalState :: (InternalState -> m a) -> ResourceT m a
withInternalState = ResourceT

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

awaitE :: Pipe l i o u m (Either u i)
awaitE = NeedInput (Done . Right) (Done . Left)
{-# RULES "conduit: awaitE >>= either" forall x y. awaitE >>= either x y = NeedInput y x #-}
{-# INLINE [1] awaitE #-}

awaitP :: Pipe l i o u m (Maybe i)
awaitP = NeedInput (Done . Just) (\_ -> Done Nothing)
{-# INLINE [1] awaitP #-}

yieldP :: o -> Pipe l i o u m ()
yieldP = HaveOutput (Done ())
{-# INLINE [1] yieldP #-}

pipe :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
pipe = goRight
  where
    goRight left right =
        case right of
            HaveOutput p o   -> HaveOutput (recurse p) o
            NeedInput rp rc  -> goLeft rp rc left
            Done r2          -> Done r2
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover _ i     -> absurd i
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

pipeL :: Monad m => Pipe l a b r0 m r1 -> Pipe b b c r1 m r2 -> Pipe l a c r0 m r2
pipeL = goRight
  where
    goRight left right =
        case right of
            HaveOutput p o    -> HaveOutput (recurse p) o
            NeedInput rp rc   -> goLeft rp rc left
            Done r2           -> Done r2
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

withUpstream :: Monad m => Pipe l i o u m r -> Pipe l i o u m (u, r)
withUpstream down = down >>= go
  where
    go r = loop
      where
        loop = awaitE >>= either (\u -> return (u, r)) (\_ -> loop)

infixr 9 <+<
infixl 9 >+>

(>+>) :: Monad m => Pipe l a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2
(>+>) = pipe
{-# INLINE (>+>) #-}

(<+<) :: Monad m => Pipe Void b c r1 m r2 -> Pipe l a b r0 m r1 -> Pipe l a c r0 m r2
(<+<) = flip pipe
{-# INLINE (<+<) #-}

catchP :: (MonadUnliftIO m, Exception e)
       => Pipe l i o u m r
       -> (e -> Pipe l i o u m r)
       -> Pipe l i o u m r
catchP p0 onErr = go p0
  where
    go (Done r) = Done r
    go (PipeM mp) = PipeM $ withRunInIO $ \run ->
      E.catch (run (liftM go mp)) (return . onErr)
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p o) = HaveOutput (go p) o
{-# INLINABLE catchP #-}

handleP :: (MonadUnliftIO m, Exception e)
        => (e -> Pipe l i o u m r)
        -> Pipe l i o u m r
        -> Pipe l i o u m r
handleP = flip catchP
{-# INLINE handleP #-}

tryP :: (MonadUnliftIO m, Exception e)
     => Pipe l i o u m r
     -> Pipe l i o u m (Either e r)
tryP p = fmap Right p `catchP` (return . Left)
{-# INLINABLE tryP #-}

generalizeUpstream :: Monad m => Pipe l i o () m r -> Pipe l i o u m r
generalizeUpstream = go
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput x y) = NeedInput (go . x) (\_ -> go (y ()))
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l
{-# INLINE generalizeUpstream #-}

newtype ConduitT i o m r = ConduitT
    { unConduitT :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

newtype SealedConduitT i o m r = SealedConduitT (Pipe i i o () m r)

type ConduitM = ConduitT

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

type Source m o = ConduitT () o m ()
{-# DEPRECATED Source "Use ConduitT directly" #-}

type Producer m o = forall i. ConduitT i o m ()
{-# DEPRECATED Producer "Use ConduitT directly" #-}

type Sink i = ConduitT i Void
{-# DEPRECATED Sink "Use ConduitT directly" #-}

type Consumer i m r = forall o. ConduitT i o m r
{-# DEPRECATED Consumer "Use ConduitT directly" #-}

type Conduit i m o = ConduitT i o m ()
{-# DEPRECATED Conduit "Use ConduitT directly" #-}

sealConduitT :: ConduitT i o m r -> SealedConduitT i o m r
sealConduitT (ConduitT f) = SealedConduitT (f Done)

unsealConduitT :: Monad m => SealedConduitT i o m r -> ConduitT i o m r
unsealConduitT (SealedConduitT f) = ConduitT (f >>=)

connectResume :: Monad m
              => SealedConduitT () a m ()
              -> ConduitT a Void m r
              -> m (SealedConduitT () a m (), r)
connectResume (SealedConduitT left0) (ConduitT right0) =
    goRight left0 (right0 Done)
  where
    goRight left right =
        case right of
            HaveOutput _ o   -> absurd o
            NeedInput rp rc  -> goLeft rp rc left
            Done r2          -> return (SealedConduitT left, r2)
            PipeM mp         -> mp >>= goRight left
            Leftover p i     -> goRight (HaveOutput left i) p

    goLeft rp rc left =
        case left of
            HaveOutput left' o            -> goRight left' (rp o)
            NeedInput _ lc                -> recurse (lc ())
            Done ()                       -> goRight (Done ()) (rc ())
            PipeM mp                      -> mp >>= recurse
            Leftover p ()                 -> recurse p
      where
        recurse = goLeft rp rc

sourceToPipe :: Monad m => ConduitT () o m () -> Pipe l i o u m ()
sourceToPipe (ConduitT k) =
    go $ k Done
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = Done ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p

sinkToPipe :: Monad m => ConduitT i Void m r -> Pipe l i o u m r
sinkToPipe (ConduitT k) =
    go $ injectLeftovers $ k Done
  where
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (Done r) = Done r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover _ l) = absurd l

conduitToPipe :: Monad m => ConduitT i o m () -> Pipe l i o u m ()
conduitToPipe (ConduitT k) =
    go $ injectLeftovers $ k Done
  where
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput p c) = NeedInput (go . p) (const $ go $ c ())
    go (Done ()) = Done ()
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover _ l) = absurd l

toProducer :: Monad m => ConduitT () a m () -> ConduitT i a m ()
toProducer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go (c ())
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p
    in go (c0 Done)

toConsumer :: Monad m => ConduitT a Void m b -> ConduitT a o m b
toConsumer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l
    in go (c0 Done)

catchC :: (MonadUnliftIO m, Exception e)
       => ConduitT i o m r
       -> (e -> ConduitT i o m r)
       -> ConduitT i o m r
catchC (ConduitT p0) onErr = ConduitT $ \rest -> let
    go (Done r) = rest r
    go (PipeM mp) = PipeM $ withRunInIO $ \ run ->
      run (liftM go mp) `E.catch` \ e ->
        return $ onErr e `unConduitT` rest
    go (Leftover p i) = Leftover (go p) i
    go (NeedInput x y) = NeedInput (go . x) (go . y)
    go (HaveOutput p o) = HaveOutput (go p) o
    in go (p0 Done)
{-# INLINE catchC #-}

handleC :: (MonadUnliftIO m, Exception e)
        => (e -> ConduitT i o m r)
        -> ConduitT i o m r
        -> ConduitT i o m r
handleC = flip catchC
{-# INLINE handleC #-}

tryC :: (MonadUnliftIO m, Exception e)
     => ConduitT i o m r
     -> ConduitT i o m (Either e r)
tryC c = fmap Right c `catchC` (return . Left)
{-# INLINE tryC #-}

zipSinks :: Monad m => ConduitT i Void m r -> ConduitT i Void m r' -> ConduitT i Void m (r, r')
zipSinks (ConduitT x0) (ConduitT y0) = ConduitT $ \rest -> let
    Leftover _  i    >< _                = absurd i
    _                >< Leftover _  i    = absurd i
    HaveOutput _ o   >< _                = absurd o
    _                >< HaveOutput _ o   = absurd o

    PipeM mx         >< y                = PipeM (liftM (>< y) mx)
    x                >< PipeM my         = PipeM (liftM (x ><) my)
    Done x           >< Done y           = rest (x, y)
    NeedInput px cx  >< NeedInput py cy  = NeedInput (\i -> px i >< py i) (\() -> cx () >< cy ())
    NeedInput px cx  >< y@Done{}         = NeedInput (\i -> px i >< y)    (\u -> cx u >< y)
    x@Done{}         >< NeedInput py cy  = NeedInput (\i -> x >< py i)    (\u -> x >< cy u)
    in injectLeftovers (x0 Done) >< injectLeftovers (y0 Done)

zipSources :: Monad m => ConduitT () a m () -> ConduitT () b m () -> ConduitT () (a, b) m ()
zipSources (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = rest ()
    go (Done ()) (HaveOutput _ _) = rest ()
    go (HaveOutput _ _) (Done ()) = rest ()
    go (Done ()) (PipeM _) = rest ()
    go (PipeM _) (Done ()) = rest ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx x) (HaveOutput srcy y) = HaveOutput (go srcx srcy) (x, y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())
    in go (left0 Done) (right0 Done)

zipSourcesApp :: Monad m => ConduitT () (a -> b) m () -> ConduitT () a m () -> ConduitT () b m ()
zipSourcesApp (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go (Leftover left ()) right = go left right
    go left (Leftover right ())  = go left right
    go (Done ()) (Done ()) = rest ()
    go (Done ()) (HaveOutput _ _) = rest ()
    go (HaveOutput _ _) (Done ()) = rest ()
    go (Done ()) (PipeM _) = rest ()
    go (PipeM _) (Done ()) = rest ()
    go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (\x -> go x y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x) my)
    go (HaveOutput srcx x) (HaveOutput srcy y) = HaveOutput (go srcx srcy) (x y)
    go (NeedInput _ c) right = go (c ()) right
    go left (NeedInput _ c) = go left (c ())
    in go (left0 Done) (right0 Done)

zipConduitApp
    :: Monad m
    => ConduitT i o m (x -> y)
    -> ConduitT i o m x
    -> ConduitT i o m y
zipConduitApp (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    go (Done f) (Done x) = rest (f x)
    go (PipeM mx) y = PipeM (flip go y `liftM` mx)
    go x (PipeM my) = PipeM (go x `liftM` my)
    go (HaveOutput x o) y = HaveOutput (go x y) o
    go x (HaveOutput y o) = HaveOutput (go x y) o
    go (Leftover _ i) _ = absurd i
    go _ (Leftover _ i) = absurd i
    go (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\i -> go (px i) (py i))
        (\u -> go (cx u) (cy u))
    go (NeedInput px cx) (Done y) = NeedInput
        (\i -> go (px i) (Done y))
        (\u -> go (cx u) (Done y))
    go (Done x) (NeedInput py cy) = NeedInput
        (\i -> go (Done x) (py i))
        (\u -> go (Done x) (cy u))
  in go (injectLeftovers $ left0 Done) (injectLeftovers $ right0 Done)

fuseReturnLeftovers :: Monad m
                    => ConduitT a b m ()
                    -> ConduitT b c m r
                    -> ConduitT a c m (r, [b])
fuseReturnLeftovers (ConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    goRight bs left right =
        case right of
            HaveOutput p o -> HaveOutput (recurse p) o
            NeedInput rp rc  ->
                case bs of
                    [] -> goLeft rp rc left
                    b:bs' -> goRight bs' left (rp b)
            Done r2          -> rest (r2, bs)
            PipeM mp         -> PipeM (liftM recurse mp)
            Leftover p b     -> goRight (b:bs) left p
      where
        recurse = goRight bs left

    goLeft rp rc left =
        case left of
            HaveOutput left' o        -> goRight [] left' (rp o)
            NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
            Done r1                   -> goRight [] (Done r1) (rc r1)
            PipeM mp                  -> PipeM (liftM recurse mp)
            Leftover left' i          -> Leftover (recurse left') i
      where
        recurse = goLeft rp rc
    in goRight [] (left0 Done) (right0 Done)

fuseLeftovers
    :: Monad m
    => ([b] -> [a])
    -> ConduitT a b m ()
    -> ConduitT b c m r
    -> ConduitT a c m r
fuseLeftovers f left right = do
    (r, bs) <- fuseReturnLeftovers left right
    mapM_ leftover $ reverse $ f bs
    return r

connectResumeConduit
    :: Monad m
    => SealedConduitT i o m ()
    -> ConduitT o Void m r
    -> ConduitT i Void m (SealedConduitT i o m (), r)
connectResumeConduit (SealedConduitT left0) (ConduitT right0) = ConduitT $ \rest -> let
    goRight left right =
        case right of
            HaveOutput _ o -> absurd o
            NeedInput rp rc -> goLeft rp rc left
            Done r2 -> rest (SealedConduitT left, r2)
            PipeM mp -> PipeM (liftM (goRight left) mp)
            Leftover p i -> goRight (HaveOutput left i) p

    goLeft rp rc left =
        case left of
            HaveOutput left' o -> goRight left' (rp o)
            NeedInput left' lc -> NeedInput (recurse . left') (recurse . lc)
            Done () -> goRight (Done ()) (rc ())
            PipeM mp -> PipeM (liftM recurse mp)
            Leftover left' i -> Leftover (recurse left') i -- recurse p
      where
        recurse = goLeft rp rc
    in goRight left0 (right0 Done)

mergeSource
  :: Monad m
  => ConduitT () i m ()
  -> ConduitT a (i, a) m ()
mergeSource = loop . sealConduitT
  where
    loop :: Monad m => SealedConduitT () i m () -> ConduitT a (i, a) m ()
    loop src0 = await >>= maybe (return ()) go
      where
        go a = do
          (src1, mi) <- lift $ src0 $$++ await
          case mi of
            Nothing -> leftover a
            Just i  -> yield (i, a) >> loop src1

passthroughSink :: Monad m
                => ConduitT i Void m r
                -> (r -> m ()) -- ^ finalizer
                -> ConduitT i i m ()
passthroughSink (ConduitT sink0) final = ConduitT $ \rest -> let

    go mbuf _ (Done r) = do
        maybe (return ()) yieldP mbuf
        lift $ final r
        unConduitT (awaitForever yield) rest
    go mbuf is (Leftover sink i) = go mbuf (i:is) sink
    go _ _ (HaveOutput _ o) = absurd o
    go mbuf is (PipeM mx) = do
        x <- lift mx
        go mbuf is x
    go mbuf (i:is) (NeedInput next _) = go mbuf is (next i)
    go mbuf [] (NeedInput next done) = do
        maybe (return ()) yieldP mbuf
        mx <- awaitP
        case mx of
            Nothing -> go Nothing [] (done ())
            Just x -> go (Just x) [] (next x)
    in go Nothing [] (sink0 Done)

sourceToList :: Monad m => ConduitT () a m () -> m [a]
sourceToList (ConduitT k) =
    go $ k Done
  where
    go (Done _) = return []
    go (HaveOutput src x) = liftM (x:) (go src)
    go (PipeM msrc) = msrc >>= go
    go (NeedInput _ c) = go (c ())
    go (Leftover p _) = go p

infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-
infixl 1 $=+
infixr 2 .|

connect :: Monad m
        => ConduitT () a m ()
        -> ConduitT a Void m r
        -> m r
connect = ($$)

unconsM :: Monad m
        => SealedConduitT () o m ()
        -> m (Maybe (o, SealedConduitT () o m ()))
unconsM (SealedConduitT p) = go p
  where
    go (HaveOutput p o) = pure $ Just (o, SealedConduitT p)
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = pure Nothing
    go (PipeM mp) = mp >>= go
    go (Leftover p ()) = go p

unconsEitherM :: Monad m
              => SealedConduitT () o m r
              -> m (Either r (o, SealedConduitT () o m r))
unconsEitherM (SealedConduitT p) = go p
  where
    go (HaveOutput p o) = pure $ Right (o, SealedConduitT p)
    go (NeedInput _ c) = go $ c ()
    go (Done r) = pure $ Left r
    go (PipeM mp) = mp >>= go
    go (Leftover p ()) = go p

fuse :: Monad m => ConduitT a b m () -> ConduitT b c m r -> ConduitT a c m r
fuse = (=$=)

(.|) :: Monad m
     => ConduitT a b m () -- ^ upstream
     -> ConduitT b c m r -- ^ downstream
     -> ConduitT a c m r
(.|) = fuse
{-# INLINE (.|) #-}

($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
    return res
{-# INLINE [1] ($$) #-}
{-# DEPRECATED ($$) "Use runConduit and .|" #-}

($=) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
($=) = (=$=)
{-# INLINE [0] ($=) #-}
{-# RULES "conduit: $= is =$=" ($=) = (=$=) #-}
{-# DEPRECATED ($=) "Use .|" #-}

(=$) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
(=$) = (=$=)
{-# INLINE [0] (=$) #-}
{-# RULES "conduit: =$ is =$=" (=$) = (=$=) #-}
{-# DEPRECATED (=$) "Use .|" #-}

(=$=) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
ConduitT left0 =$= ConduitT right0 = ConduitT $ \rest ->
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
{-# INLINE [1] (=$=) #-}
{-# DEPRECATED (=$=) "Use .|" #-}

await :: Monad m => ConduitT i o m (Maybe i)
await = ConduitT $ \f -> NeedInput (f . Just) (const $ f Nothing)
{-# INLINE [0] await #-}

await' :: Monad m
       => ConduitT i o m r
       -> (i -> ConduitT i o m r)
       -> ConduitT i o m r
await' f g = ConduitT $ \rest -> NeedInput
    (\i -> unConduitT (g i) rest)
    (const $ unConduitT f rest)
{-# INLINE await' #-}
{-# RULES "conduit: await >>= maybe" forall x y. await >>= maybe x y = await' x y #-}

yield :: Monad m
      => o -- ^ output value
      -> ConduitT i o m ()
yield o = ConduitT $ \rest -> HaveOutput (rest ()) o
{-# INLINE yield #-}

yieldM :: Monad m => m o -> ConduitT i o m ()
yieldM mo = lift mo >>= yield
{-# INLINE yieldM #-}

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

awaitForever :: Monad m => (i -> ConduitT i o m r) -> ConduitT i o m ()
awaitForever f = ConduitT $ \rest ->
    let go = NeedInput (\i -> unConduitT (f i) (const go)) rest
     in go

transPipe :: Monad m => (forall a. m a -> n a) -> ConduitT i o m r -> ConduitT i o n r
transPipe f (ConduitT c0) = ConduitT $ \rest -> let
        go (HaveOutput p o) = HaveOutput (go p) o
        go (NeedInput p c) = NeedInput (go . p) (go . c)
        go (Done r) = rest r
        go (PipeM mp) =
            PipeM (f $ liftM go $ collapse mp)
          where
            collapse mpipe = do
                pipe' <- mpipe
                case pipe' of
                    PipeM mpipe' -> collapse mpipe'
                    _ -> return pipe'
        go (Leftover p i) = Leftover (go p) i
        in go (c0 Done)

mapOutput :: Monad m => (o1 -> o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutput f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) (f o)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutputMaybe f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = maybe id (\o' p' -> HaveOutput p' o') (f o) (go p)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

mapInput :: Monad m
         => (i1 -> i2) -- ^ map initial input to new input
         -> (i2 -> Maybe i1) -- ^ map new leftovers to initial leftovers
         -> ConduitT i2 o m r
         -> ConduitT i1 o m r
mapInput f f' (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput p c) = NeedInput (go . p . f) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM $ liftM go mp
    go (Leftover p i) = maybe id (flip Leftover) (f' i) (go p)
    in go (c0 Done)

mapInputM :: Monad m
          => (i1 -> m i2) -- ^ map initial input to new input
          -> (i2 -> m (Maybe i1)) -- ^ map new leftovers to initial leftovers
          -> ConduitT i2 o m r
          -> ConduitT i1 o m r
mapInputM f f' (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput p c)  = NeedInput (\i -> PipeM $ go . p <$> f i) (go . c)
    go (Done r)         = rest r
    go (PipeM mp)       = PipeM $ fmap go mp
    go (Leftover p i)   = PipeM $ (\x -> maybe id (flip Leftover) x (go p)) <$> f' i
    in go (c0 Done)

($$+) :: Monad m => ConduitT () a m () -> ConduitT a Void m b -> m (SealedConduitT () a m (), b)
src $$+ sink = connectResume (sealConduitT src) sink
{-# INLINE ($$+) #-}

($$++) :: Monad m => SealedConduitT () a m () -> ConduitT a Void m b -> m (SealedConduitT () a m (), b)
($$++) = connectResume
{-# INLINE ($$++) #-}

($$+-) :: Monad m => SealedConduitT () a m () -> ConduitT a Void m b -> m b
rsrc $$+- sink = do
    (_, res) <- connectResume rsrc sink
    return res
{-# INLINE ($$+-) #-}

($=+) :: Monad m => SealedConduitT () a m () -> ConduitT a b m () -> SealedConduitT () b m ()
SealedConduitT src $=+ ConduitT sink = SealedConduitT (src `pipeL` sink Done)

data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)

newtype ZipSource m o = ZipSource { getZipSource :: ConduitT () o m () }

instance Monad m => Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource
instance Monad m => Applicative (ZipSource m) where
    pure  = ZipSource . forever . yield
    (ZipSource f) <*> (ZipSource x) = ZipSource $ zipSourcesApp f x

sequenceSources :: (Traversable f, Monad m) => f (ConduitT () o m ()) -> ConduitT () (f o) m ()
sequenceSources = getZipSource . sequenceA . fmap ZipSource

newtype ZipSink i m r = ZipSink { getZipSink :: ConduitT i Void m r }

instance Monad m => Functor (ZipSink i m) where
    fmap f (ZipSink x) = ZipSink (liftM f x)
instance Monad m => Applicative (ZipSink i m) where
    pure  = ZipSink . return
    (ZipSink f) <*> (ZipSink x) =
         ZipSink $ liftM (uncurry ($)) $ zipSinks f x

sequenceSinks :: (Traversable f, Monad m) => f (ConduitT i Void m r) -> ConduitT i Void m (f r)
sequenceSinks = getZipSink . sequenceA . fmap ZipSink

(=$$+) :: Monad m
       => ConduitT a b m ()
       -> ConduitT b Void m r
       -> ConduitT a Void m (SealedConduitT a b m (), r)
(=$$+) conduit = connectResumeConduit (sealConduitT conduit)
{-# INLINE (=$$+) #-}

(=$$++) :: Monad m => SealedConduitT i o m () -> ConduitT o Void m r -> ConduitT i Void m (SealedConduitT i o m (), r)
(=$$++) = connectResumeConduit
{-# INLINE (=$$++) #-}

(=$$+-) :: Monad m => SealedConduitT i o m () -> ConduitT o Void m r -> ConduitT i Void m r
rsrc =$$+- sink = do
    (_, res) <- connectResumeConduit rsrc sink
    return res
{-# INLINE (=$$+-) #-}

infixr 0 =$$+
infixr 0 =$$++
infixr 0 =$$+-

newtype ZipConduit i o m r = ZipConduit { getZipConduit :: ConduitT i o m r }
    deriving Functor
instance Monad m => Applicative (ZipConduit i o m) where
    pure = ZipConduit . pure
    ZipConduit left <*> ZipConduit right = ZipConduit (zipConduitApp left right)

sequenceConduits :: (Traversable f, Monad m) => f (ConduitT i o m r) -> ConduitT i o m (f r)
sequenceConduits = getZipConduit . sequenceA . fmap ZipConduit

fuseBoth :: Monad m => ConduitT a b m r1 -> ConduitT b c m r2 -> ConduitT a c m (r1, r2)
fuseBoth (ConduitT up) (ConduitT down) =
    ConduitT (pipeL (up Done) (withUpstream $ generalizeUpstream $ down Done) >>=)
{-# INLINE fuseBoth #-}

fuseBothMaybe
    :: Monad m
    => ConduitT a b m r1
    -> ConduitT b c m r2
    -> ConduitT a c m (Maybe r1, r2)
fuseBothMaybe (ConduitT up) (ConduitT down) =
    ConduitT (pipeL (up Done) (go Nothing $ down Done) >>=)
  where
    go mup (Done r) = Done (mup, r)
    go mup (PipeM mp) = PipeM $ liftM (go mup) mp
    go mup (HaveOutput p o) = HaveOutput (go mup p) o
    go _ (NeedInput p c) = NeedInput
        (\i -> go Nothing (p i))
        (\u -> go (Just u) (c ()))
    go mup (Leftover p i) = Leftover (go mup p) i
{-# INLINABLE fuseBothMaybe #-}

fuseUpstream :: Monad m => ConduitT a b m r -> ConduitT b c m () -> ConduitT a c m r
fuseUpstream up down = fmap fst (fuseBoth up down)
{-# INLINE fuseUpstream #-}

{- FIXME
{-# RULES "conduit: ConduitT: lift x >>= f" forall m f. lift m >>= f = ConduitT (PipeM (liftM (unConduitT . f) m)) #-}
{-# RULES "conduit: ConduitT: lift x >> f" forall m f. lift m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) m)) #-}

{-# RULES "conduit: ConduitT: liftIO x >>= f" forall m (f :: MonadIO m => a -> ConduitT i o m r). liftIO m >>= f = ConduitT (PipeM (liftM (unConduitT . f) (liftIO m))) #-}
{-# RULES "conduit: ConduitT: liftIO x >> f" forall m (f :: MonadIO m => ConduitT i o m r). liftIO m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) (liftIO m))) #-}

{-# RULES "conduit: ConduitT: liftBase x >>= f" forall m (f :: MonadBase b m => a -> ConduitT i o m r). liftBase m >>= f = ConduitT (PipeM (liftM (unConduitT . f) (liftBase m))) #-}
{-# RULES "conduit: ConduitT: liftBase x >> f" forall m (f :: MonadBase b m => ConduitT i o m r). liftBase m >> f = ConduitT (PipeM (liftM (\_ -> unConduitT f) (liftBase m))) #-}

{-# RULES
    "yield o >> p" forall o (p :: ConduitT i o m r). yield o >> p = ConduitT (HaveOutput (unConduitT p) o)
  ; "when yield next" forall b o p. when b (yield o) >> p =
        if b then ConduitT (HaveOutput (unConduitT p) o) else p
  ; "unless yield next" forall b o p. unless b (yield o) >> p =
        if b then p else ConduitT (HaveOutput (unConduitT p) o)
  ; "lift m >>= yield" forall m. lift m >>= yield = yieldM m
   #-}
{-# RULES "conduit: leftover l >> p" forall l (p :: ConduitT i o m r). leftover l >> p =
    ConduitT (Leftover (unConduitT p) l) #-}
    -}

runConduitPure :: ConduitT () Void Identity r -> r
runConduitPure = runIdentity . runConduit
{-# INLINE runConduitPure #-}

runConduitRes :: MonadUnliftIO m
              => ConduitT () Void (ResourceT m) r
              -> m r
runConduitRes = runResourceT . runConduit
{-# INLINE runConduitRes #-}

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
