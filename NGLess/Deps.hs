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

module Deps where

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
import           Prelude hiding (lines)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Prelude hiding (takeWhile)
import qualified System.IO as IO

-- BEGIN NGLess/Control/Monad/Cont/Class.hs


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

-- | @since 2.3.1
instance forall k (r :: k) (m :: (k -> Type)) . MonadCont (ContT r m) where
    callCC = ContT.callCC

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

-- | @since 2.2
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

-- | @since 2.3
instance (Monoid w, MonadCont m) => MonadCont (CPSRWS.RWST r w s m) where
    callCC = CPSRWS.liftCallCC' callCC

-- | @since 2.3
instance (Monoid w, MonadCont m) => MonadCont (CPSWriter.WriterT w m) where
    callCC = CPSWriter.liftCallCC callCC

-- | @since 2.3
instance
  ( Monoid w
  , MonadCont m
  ) => MonadCont (AccumT w m) where
    callCC = Accum.liftCallCC callCC

-- BEGIN NGLess/Control/Monad/Error/Class.hs


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
    -- | Is used within a monadic computation to begin exception processing.
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

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

{- | @since 2.2 -}
instance Monad m => MonadError e (ExceptT e m) where
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

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

-- | @since 2.3
instance (Monoid w, MonadError e m) => MonadError e (CPSRWS.RWST r w s m) where
    throwError = lift . throwError
    catchError = CPSRWS.liftCatch catchError

-- | @since 2.3
instance (Monoid w, MonadError e m) => MonadError e (CPSWriter.WriterT w m) where
    throwError = lift . throwError
    catchError = CPSWriter.liftCatch catchError

-- | @since 2.3
instance
  ( Monoid w
  , MonadError e m
  ) => MonadError e (AccumT w m) where
    throwError = lift . throwError
    catchError = Accum.liftCatch catchError

-- BEGIN NGLess/Control/Monad/IO/Unlift.hs


-- | The ability to run any monadic action @m a@ as @IO a@.
--
-- This is more precisely a natural transformation. We need to new
-- datatype (instead of simply using a @forall@) due to lack of
-- support in GHC for impredicative types.
--
-- @since 0.1.0.0
newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

-- | Monads which allow their actions to be run in 'IO'.
--
-- While 'MonadIO' allows an 'IO' action to be lifted into another
-- monad, this class captures the opposite concept: allowing you to
-- capture the monadic context. Note that, in order to meet the laws
-- given below, the intuition is that a monad must have no monadic
-- state, but may have monadic context. This essentially limits
-- 'MonadUnliftIO' to 'ReaderT' and 'IdentityT' transformers on top of
-- 'IO'.
--
-- Laws. For any function @run@ provided by 'withRunInIO', it must meet the
-- monad transformer laws as reformulated for @MonadUnliftIO@:
--
-- * @run . return = return@
--
-- * @run (m >>= f) = run m >>= run . f@
--
-- Instances of @MonadUnliftIO@ must also satisfy the following laws:
--
-- [Identity law] @withRunInIO (\\run -> run m) = m@
-- [Inverse law]  @withRunInIO (\\_ -> m) = liftIO m@
--
-- As an example of an invalid instance, a naive implementation of
-- @MonadUnliftIO (StateT s m)@ might be
--
-- @
-- withRunInIO inner =
--   StateT $ \\s ->
--     withRunInIO $ \\run ->
--       inner (run . flip evalStateT s)
-- @
--
-- This breaks the identity law because the inner @run m@ would throw away
-- any state changes in @m@.
--
-- @since 0.1.0.0
class MonadIO m => MonadUnliftIO m where
  -- | Convenience function for capturing the monadic context and running an 'IO'
  -- action with a runner function. The runner function is used to run a monadic
  -- action @m@ in @IO@.
  --
  -- @since 0.1.0.0
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

-- | Capture the current monadic context, providing the ability to
-- run monadic actions in 'IO'.
--
-- See 'UnliftIO' for an explanation of why we need a helper
-- datatype here.
--
-- Prior to version 0.2.0.0 of this library, this was a method in the
-- 'MonadUnliftIO' type class. It was moved out due to
-- <https://github.com/fpco/unliftio/issues/55>.
--
-- @since 0.1.0.0
askUnliftIO :: MonadUnliftIO m => m (UnliftIO m)
askUnliftIO = withRunInIO (\run -> return (UnliftIO run))
{-# INLINE askUnliftIO #-}
-- Would be better, but GHC hates us
-- askUnliftIO :: m (forall a. m a -> IO a)


-- | Same as 'askUnliftIO', but returns a monomorphic function
-- instead of a polymorphic newtype wrapper. If you only need to apply
-- the transformation on one concrete type, this function can be more
-- convenient.
--
-- @since 0.1.0.0
{-# INLINE askRunInIO #-}
askRunInIO :: MonadUnliftIO m => m (m a -> IO a)
-- withRunInIO return would be nice, but GHC 7.8.4 doesn't like it
askRunInIO = withRunInIO (\run -> (return (\ma -> run ma)))

-- | Convenience function for capturing the monadic context and running
-- an 'IO' action. The 'UnliftIO' newtype wrapper is rarely needed, so
-- prefer 'withRunInIO' to this function.
--
-- @since 0.1.0.0
{-# INLINE withUnliftIO #-}
withUnliftIO :: MonadUnliftIO m => (UnliftIO m -> IO a) -> m a
withUnliftIO inner = askUnliftIO >>= liftIO . inner

-- | Convert an action in @m@ to an action in @IO@.
--
-- @since 0.1.0.0
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
                   -- ^ The wrapper, for instance @IdentityT@.
                   -> (forall a. m a -> n a)
                   -- ^ The inverse, for instance @runIdentityT@.
                   -> ((forall a. m a -> IO a) -> IO b)
                   -- ^ The actual function to invoke 'withRunInIO' with.
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

-- BEGIN NGLess/Control/Monad/Primitive.hs


#if __GLASGOW_HASKELL__ < 802
type UnliftedType = TYPE 'PtrRepUnlifted
#elif __GLASGOW_HASKELL__ < 902
type UnliftedType = TYPE 'UnliftedRep
#endif

-- | Class of monads which can perform primitive state-transformer actions.
class Monad m => PrimMonad m where
  -- | State token type.
  type PrimState m

  -- | Execute a primitive operation.
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

-- | Class of primitive monads for state-transformer actions.
--
-- Unlike 'PrimMonad', this typeclass requires that the @Monad@ be fully
-- expressed as a state transformer, therefore disallowing other monad
-- transformers on top of the base @IO@ or @ST@.
--
-- @since 0.6.0.0
class PrimMonad m => PrimBase m where
  -- | Expose the internal structure of the monad.
  internal :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

-- | Execute a primitive operation with no result.
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

-- | @since 0.6.3.0
instance PrimMonad m => PrimMonad (ContT r m) where
  type PrimState (ContT r m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (IdentityT m) where
  type PrimState (IdentityT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

-- | @since 0.6.2.0
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
-- | @since 0.6.3.0
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

-- see https://gitlab.haskell.org/ghc/ghc/commit/2f5cb3d44d05e581b75a47fec222577dfa7a533e
-- for why we only support an instance for ghc >= 8.2
#if __GLASGOW_HASKELL__ >= 802
-- @since 0.7.1.0
instance PrimMonad (L.ST s) where
  type PrimState (L.ST s) = s
  primitive = L.strictToLazyST . primitive
  {-# INLINE primitive #-}

-- @since 0.7.1.0
instance PrimBase (L.ST s) where
  internal = internal . L.lazyToStrictST
  {-# INLINE internal #-}
#endif

-- | 'PrimMonad'\'s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimMonad m, s ~ PrimState m) => MonadPrim s m
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

-- | 'PrimBase'\'s state token type can be annoying to handle
--   in constraints. This typeclass lets users (visually) notice
--   'PrimState' equality constraints less, by witnessing that
--   @s ~ 'PrimState' m@.
class (PrimBase m, MonadPrim s m) => MonadPrimBase s m
instance (PrimBase m, MonadPrim s m) => MonadPrimBase s m

-- | Lifts a 'PrimBase' into another 'PrimMonad' with the same underlying state
-- token type.
liftPrim
  :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2) => m1 a -> m2 a
{-# INLINE liftPrim #-}
liftPrim = primToPrim

-- | Convert a 'PrimBase' to another monad with the same state token.
primToPrim :: (PrimBase m1, PrimMonad m2, PrimState m1 ~ PrimState m2)
        => m1 a -> m2 a
{-# INLINE primToPrim #-}
primToPrim m = primitive (internal m)

-- | Convert a 'PrimBase' with a 'RealWorld' state token to 'IO'
primToIO :: (PrimBase m, PrimState m ~ RealWorld) => m a -> IO a
{-# INLINE primToIO #-}
primToIO = primToPrim

-- | Convert a 'PrimBase' to 'ST'
primToST :: PrimBase m => m a -> ST (PrimState m) a
{-# INLINE primToST #-}
primToST = primToPrim

-- | Convert an 'IO' action to a 'PrimMonad'.
--
-- @since 0.6.2.0
ioToPrim :: (PrimMonad m, PrimState m ~ RealWorld) => IO a -> m a
{-# INLINE ioToPrim #-}
ioToPrim = primToPrim

-- | Convert an 'ST' action to a 'PrimMonad'.
--
-- @since 0.6.2.0
stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
{-# INLINE stToPrim #-}
stToPrim = primToPrim

-- | Convert a 'PrimBase' to another monad with a possibly different state
-- token. This operation is highly unsafe!
unsafePrimToPrim :: (PrimBase m1, PrimMonad m2) => m1 a -> m2 a
{-# INLINE unsafePrimToPrim #-}
unsafePrimToPrim m = primitive (unsafeCoerce# (internal m))

-- | Convert any 'PrimBase' to 'ST' with an arbitrary state token. This
-- operation is highly unsafe!
unsafePrimToST :: PrimBase m => m a -> ST s a
{-# INLINE unsafePrimToST #-}
unsafePrimToST = unsafePrimToPrim

-- | Convert any 'PrimBase' to 'IO'. This operation is highly unsafe!
unsafePrimToIO :: PrimBase m => m a -> IO a
{-# INLINE unsafePrimToIO #-}
unsafePrimToIO = unsafePrimToPrim

-- | Convert an 'ST' action with an arbitrary state token to any 'PrimMonad'.
-- This operation is highly unsafe!
--
-- @since 0.6.2.0
unsafeSTToPrim :: PrimMonad m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

-- | Convert an 'IO' action to any 'PrimMonad'. This operation is highly
-- unsafe!
--
-- @since 0.6.2.0
unsafeIOToPrim :: PrimMonad m => IO a -> m a
{-# INLINE unsafeIOToPrim #-}
unsafeIOToPrim = unsafePrimToPrim

-- | See 'unsafeInlineIO'. This function is not recommended for the same
-- reasons.
unsafeInlinePrim :: PrimBase m => m a -> a
{-# INLINE unsafeInlinePrim #-}
unsafeInlinePrim m = unsafeInlineIO (unsafePrimToIO m)

-- | Generally, do not use this function. It is the same as
-- @accursedUnutterablePerformIO@ from @bytestring@ and is well behaved under
-- narrow conditions. See the documentation of that function to get an idea
-- of when this is sound. In most cases @GHC.IO.Unsafe.unsafeDupablePerformIO@
-- should be preferred.
unsafeInlineIO :: IO a -> a
{-# INLINE unsafeInlineIO #-}
unsafeInlineIO m = case internal m realWorld# of (# _, r #) -> r

-- | See 'unsafeInlineIO'. This function is not recommended for the same
-- reasons. Prefer @runST@ when @s@ is free.
unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

-- | Ensure that the value is considered alive by the garbage collection.
-- Warning: GHC has optimization passes that can erase @touch@ if it is
-- certain that an exception is thrown afterward. Prefer 'keepAlive'.
touch :: PrimMonad m => a -> m ()
{-# INLINE touch #-}
touch x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

-- | Variant of 'touch' that keeps a value of an unlifted type
-- (e.g. @MutableByteArray#@) alive.
touchUnlifted :: forall (m :: Type -> Type) (a :: UnliftedType). PrimMonad m => a -> m ()
{-# INLINE touchUnlifted #-}
touchUnlifted x = unsafePrimToPrim
        $ (primitive (\s -> case touch# x s of { s' -> (# s', () #) }) :: IO ())

-- | Keep value @x@ alive until computation @k@ completes.
-- Warning: This primop exists for completeness, but it is difficult to use
-- correctly. Prefer 'keepAliveUnlifted' if the value to keep alive is simply
-- a wrapper around an unlifted type (e.g. @ByteArray@).
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

-- | Variant of 'keepAlive' in which the value kept alive is of an unlifted
-- boxed type.
keepAliveUnlifted :: forall (m :: Type -> Type) (a :: UnliftedType) (r :: Type). PrimBase m => a -> m r -> m r
#if defined(HAVE_KEEPALIVE)
{-# INLINE keepAliveUnlifted #-}
keepAliveUnlifted x k =
  primitive $ \s0 -> keepAliveUnliftedLifted# x s0 (internal k)

#else
{-# NOINLINE keepAliveUnlifted #-}
keepAliveUnlifted x k = k <* touchUnlifted x
#endif

-- | Create an action to force a value; generalizes 'Control.Exception.evaluate'
--
-- @since 0.6.2.0
evalPrim :: forall a m . PrimMonad m => a -> m a
evalPrim a = primitive (\s -> seq# a s)

noDuplicate :: PrimMonad m => m ()
#if __GLASGOW_HASKELL__ >= 802
noDuplicate = primitive $ \ s -> (# noDuplicate# s, () #)
#else
-- noDuplicate# was limited to RealWorld
noDuplicate = unsafeIOToPrim $ primitive $ \s -> (# noDuplicate# s, () #)
#endif

unsafeInterleave, unsafeDupableInterleave :: PrimBase m => m a -> m a
unsafeInterleave x = unsafeDupableInterleave (noDuplicate >> x)
unsafeDupableInterleave x = primitive $ \ s -> let r' = case internal x s of (# _, r #) -> r in (# s, r' #)
{-# INLINE unsafeInterleave #-}
{-# NOINLINE unsafeDupableInterleave #-}
-- See Note [unsafeDupableInterleaveIO should not be inlined]
-- in GHC.IO.Unsafe

-- BEGIN NGLess/Control/Monad/RWS/Class.hs



class (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m)
   => MonadRWS r w s m | m -> r, m -> w, m -> s

-- | @since 2.3
instance (Monoid w, Monad m) => MonadRWS r w s (CPSRWS.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Lazy.RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS r w s (Strict.RWST r w s m)

---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.2
instance MonadRWS r w s m => MonadRWS r w s (ExceptT e m)
instance MonadRWS r w s m => MonadRWS r w s (IdentityT m)
instance MonadRWS r w s m => MonadRWS r w s (MaybeT m)

-- BEGIN NGLess/Control/Monad/Reader/Class.hs


-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

-- | See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    -- | Retrieves the monad environment.
    ask   :: m r
    ask = reader id

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = do
      r <- ask
      return (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT.ask
    local = ReaderT.local
    reader = ReaderT.reader

-- | @since 2.3
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

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

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

-- | @since 2.3
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

-- | @since 2.3
instance
  ( Monoid w
  , MonadReader r m
  ) => MonadReader r (AccumT w m) where
    ask = lift ask
    local = Accum.mapAccumT . local
    reader = lift . reader

-- | @since 2.3
instance
  ( MonadReader r' m
  ) => MonadReader r' (SelectT r m) where
    ask = lift ask
    -- there is no Select.liftLocal
    local f m = SelectT $ \c -> do
      r <- ask
      local f (runSelectT m (local (const r) . c))
    reader = lift . reader

-- BEGIN NGLess/Control/Monad/State/Class.hs


-- ---------------------------------------------------------------------------

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
    {-# MINIMAL state | get, put #-}

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.
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

-- | @since 2.3
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

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.2
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

-- | @since 2.3
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

-- | @since 2.3
instance
  ( Monoid w
  , MonadState s m
  ) => MonadState s (AccumT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | @since 2.3
instance MonadState s m => MonadState s (SelectT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- BEGIN NGLess/Control/Monad/State/Lazy.hs



-- ---------------------------------------------------------------------------
-- $examples
-- A function to increment a counter.  Taken from the paper
-- /Generalising Monads to Arrows/, John
-- Hughes (<http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf>), November 1998:
--
-- > tick :: State Int Int
-- > tick = do n <- get
-- >           put (n+1)
-- >           return n
--
-- Add one to the given number using the state monad:
--
-- > plusOne :: Int -> Int
-- > plusOne n = execState tick n
--
-- A contrived addition example. Works only with positive numbers:
--
-- > plus :: Int -> Int -> Int
-- > plus n x = execState (sequence $ replicate n tick) x
--
-- An example from /The Craft of Functional Programming/, Simon
-- Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
-- Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
-- tree of integers in which the original elements are replaced by
-- natural numbers, starting from 0.  The same element has to be
-- replaced by the same number at every occurrence, and when we meet
-- an as-yet-unvisited element we have to find a \'new\' number to match
-- it with:\"
--
-- > data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- > type Table a = [a]
--
-- > numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
-- > numberTree Nil = return Nil
-- > numberTree (Node x t1 t2)
-- >        =  do num <- numberNode x
-- >              nt1 <- numberTree t1
-- >              nt2 <- numberTree t2
-- >              return (Node num nt1 nt2)
-- >     where
-- >     numberNode :: Eq a => a -> State (Table a) Int
-- >     numberNode x
-- >        = do table <- get
-- >             (newTable, newPos) <- return (nNode x table)
-- >             put newTable
-- >             return newPos
-- >     nNode::  (Eq a) => a -> Table a -> (Table a, Int)
-- >     nNode x table
-- >        = case (findIndexInList (== x) table) of
-- >          Nothing -> (table ++ [x], length table)
-- >          Just i  -> (table, i)
-- >     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
-- >     findIndexInList = findIndexInListHelp 0
-- >     findIndexInListHelp _ _ [] = Nothing
-- >     findIndexInListHelp count f (h:t)
-- >        = if (f h)
-- >          then Just count
-- >          else findIndexInListHelp (count+1) f t
--
-- numTree applies numberTree with an initial state:
--
-- > numTree :: (Eq a) => Tree a -> Tree Int
-- > numTree t = evalState (numberTree t) []
--
-- > testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
-- > numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
--
-- sumTree is a little helper function that does not use the State monad:
--
-- > sumTree :: (Num a) => Tree a -> a
-- > sumTree Nil = 0
-- > sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)

-- BEGIN NGLess/Control/Monad/Trans/Resource.hs
#if !MIN_VERSION_transformers(0,6,0)
#endif

-- | The way in which a release is called.
data ReleaseType
    = ReleaseEarly
    | ReleaseNormal
    | ReleaseExceptionWith E.SomeException
    deriving (Show, Typeable)

-- | A @Monad@ which allows for safe resource allocation. In theory, any monad
-- transformer stack which includes a @ResourceT@ can be an instance of
-- @MonadResource@.
--
-- Note: @runResourceT@ has a requirement for a @MonadUnliftIO m@ monad,
-- which allows control operations to be lifted. A @MonadResource@ does not
-- have this requirement. This means that transformers such as @ContT@ can be
-- an instance of @MonadResource@. However, the @ContT@ wrapper will need to be
-- unwrapped before calling @runResourceT@.
--
-- Since 0.3.0
class MonadIO m => MonadResource m where
    -- | Lift a @ResourceT IO@ action into the current @Monad@.
    --
    -- Since 0.4.0
    liftResourceT :: ResourceT IO a -> m a


-- | A lookup key for a specific release action. This value is returned by
-- 'register' and 'allocate', and is passed to 'release'.
--
-- Since 0.3.0
data ReleaseKey = ReleaseKey !(I.IORef ReleaseMap) !Int
    deriving Typeable

type RefCount = Word
type NextKey = Int

data ReleaseMap =
    ReleaseMap !NextKey !RefCount !(IntMap (ReleaseType -> IO ()))
  | ReleaseMapClosed

-- | Convenient alias for @ResourceT IO@.
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

-- | Transform the monad a @ResourceT@ lives in. This is most often used to
-- strip or add new transformers to a stack, e.g. to run a @ReaderT@.
--
-- Note that this function is a slight generalization of 'hoist'.
--
-- Since 0.3.0
transResourceT :: (m a -> n b)
               -> ResourceT m a
               -> ResourceT n b
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))

-- | The Resource transformer. This transformer keeps track of all registered
-- actions, and calls them upon exit (via 'runResourceT'). Actions may be
-- registered via 'register', or resources may be allocated atomically via
-- 'allocate'. @allocate@ corresponds closely to @bracket@.
--
-- Releasing may be performed before exit via the 'release' function. This is a
-- highly recommended optimization, as it will ensure that scarce resources are
-- freed early. Note that calling @release@ will deregister the action, so that
-- a release action will only ever be called once.
--
-- Since 0.3.0
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

-- | Indicates either an error in the library, or misuse of it (e.g., a
-- @ResourceT@'s state is accessed after being released).
--
-- Since 0.3.0
data InvalidAccess = InvalidAccess { functionName :: String }
    deriving Typeable

instance Show InvalidAccess where
    show (InvalidAccess f) = concat
        [ "Control.Monad.Trans.Resource."
        , f
        , ": The mutable state is being accessed after cleanup. Please contact the maintainers."
        ]

instance Exception InvalidAccess

-------- All of our monad et al instances
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

-- | Since 1.1.5
instance Alternative m => Alternative (ResourceT m) where
    empty = ResourceT $ \_ -> empty
    (ResourceT mf) <|> (ResourceT ma) = ResourceT $ \r -> mf r <|> ma r

-- | Since 1.1.5
instance MonadPlus m => MonadPlus (ResourceT m) where
    mzero = ResourceT $ \_ -> mzero
    (ResourceT mf) `mplus` (ResourceT ma) = ResourceT $ \r -> mf r `mplus` ma r

instance Monad m => Monad (ResourceT m) where
    return = pure
    ResourceT ma >>= f = ResourceT $ \r -> do
        a <- ma r
        let ResourceT f' = f a
        f' r

-- | @since 1.2.2
instance MonadFail m => MonadFail (ResourceT m) where
    fail = lift . Control.Monad.Fail.fail

-- | @since 1.1.8
instance MonadFix m => MonadFix (ResourceT m) where
  mfix f = ResourceT $ \r -> mfix $ \a -> unResourceT (f a) r

instance MonadTrans ResourceT where
    lift = ResourceT . const

instance MonadIO m => MonadIO (ResourceT m) where
    liftIO = lift . liftIO

-- | @since 1.1.10
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

-- |
--
-- Since 1.1.2
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

-- | Thrown when one or more cleanup functions themselves throw an
-- exception during cleanup.
--
-- @since 1.1.11
data ResourceCleanupException = ResourceCleanupException
  { rceOriginalException :: !(Maybe SomeException)
  -- ^ If the 'ResourceT' block exited due to an exception, this is
  -- that exception.
  --
  -- @since 1.1.11
  , rceFirstCleanupException :: !SomeException
  -- ^ The first cleanup exception. We keep this separate from
  -- 'rceOtherCleanupExceptions' to prove that there's at least one
  -- (i.e., a non-empty list).
  --
  -- @since 1.1.11
  , rceOtherCleanupExceptions :: ![SomeException]
  -- ^ All other exceptions in cleanups.
  --
  -- @since 1.1.11
  }
  deriving (Show, Typeable)
instance Exception ResourceCleanupException

-- | Clean up a release map, but throw a 'ResourceCleanupException' if
-- anything goes wrong in the cleanup handlers.
--
-- @since 1.1.11
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

-- Note that this returns values in reverse order, which is what we
-- want in the specific case of this function.
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

-- | Register some action that will be called precisely once, either when
-- 'runResourceT' is called, or when the 'ReleaseKey' is passed to 'release'.
--
-- Since 0.3.0
register :: MonadResource m => IO () -> m ReleaseKey
register = liftResourceT . registerRIO

-- | Call a release action early, and deregister it from the list of cleanup
-- actions to be performed.
--
-- Since 0.3.0
release :: MonadIO m => ReleaseKey -> m ()
release (ReleaseKey istate rk) = liftIO $ release' istate rk
    (maybe (return ()) id)

-- | Unprotect resource from cleanup actions; this allows you to send
-- resource into another resourcet process and reregister it there.
-- It returns a release action that should be run in order to clean
-- resource or Nothing in case if resource is already freed.
--
-- Since 0.4.5
unprotect :: MonadIO m => ReleaseKey -> m (Maybe (IO ()))
unprotect (ReleaseKey istate rk) = liftIO $ release' istate rk return

-- | Perform some allocation, and automatically register a cleanup action.
--
-- This is almost identical to calling the allocation and then
-- @register@ing the release action, but this properly handles masking of
-- asynchronous exceptions.
--
-- Since 0.3.0
allocate :: MonadResource m
         => IO a -- ^ allocate
         -> (a -> IO ()) -- ^ free resource
         -> m (ReleaseKey, a)
allocate a = liftResourceT . allocateRIO a

-- | Perform some allocation where the return value is not required, and
-- automatically register a cleanup action.
--
-- @allocate_@ is to @allocate@ as @bracket_@ is to @bracket@
--
-- This is almost identical to calling the allocation and then
-- @register@ing the release action, but this properly handles masking of
-- asynchronous exceptions.
--
-- @since 1.2.4
allocate_ :: MonadResource m
          => IO a -- ^ allocate
          -> IO () -- ^ free resource
          -> m ReleaseKey
allocate_ a = fmap fst . allocate a . const

-- | Perform asynchronous exception masking.
--
-- This is more general then @Control.Exception.mask@, yet more efficient
-- than @Control.Exception.Lifted.mask@.
--
-- Since 0.3.0
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
    -- We tried to call release, but since the state is already closed, we
    -- can assume that the release action was already called. Previously,
    -- this threw an exception, though given that @release@ can be called
    -- from outside the context of a @ResourceT@ starting with version
    -- 0.4.4, it's no longer a library misuse or a library bug.
    lookupAction ReleaseMapClosed = (ReleaseMapClosed, Nothing)



-- | Unwrap a 'ResourceT' transformer, and call all registered release actions.
--
-- Note that there is some reference counting involved due to 'resourceForkIO'.
-- If multiple threads are sharing the same collection of resources, only the
-- last call to @runResourceT@ will deallocate the resources.
--
-- /NOTE/ Since version 1.2.0, this function will throw a
-- 'ResourceCleanupException' if any of the cleanup functions throw an
-- exception.
--
-- @since 0.3.0
runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT (ResourceT r) = withRunInIO $ \run -> do
    istate <- createInternalState
    E.mask $ \restore -> do
        res <- restore (run (r istate)) `E.catch` \e -> do
            stateCleanupChecked (Just e) istate
            E.throwIO e
        stateCleanupChecked Nothing istate
        return res

-- | Backwards compatible alias for 'runResourceT'.
--
-- @since 1.1.11
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

-- | This function mirrors @join@ at the transformer level: it will collapse
-- two levels of @ResourceT@ into a single @ResourceT@.
--
-- Since 0.4.6
joinResourceT :: ResourceT (ResourceT m) a
              -> ResourceT m a
joinResourceT (ResourceT f) = ResourceT $ \r -> unResourceT (f r) r

-- | Introduce a reference-counting scheme to allow a resource context to be
-- shared by multiple threads. Once the last thread exits, all remaining
-- resources will be released.
--
-- The first parameter is a function which will be used to create the
-- thread, such as @forkIO@ or @async@.
--
-- Note that abuse of this function will greatly delay the deallocation of
-- registered resources. This function should be used with care. A general
-- guideline:
--
-- If you are allocating a resource that should be shared by multiple threads,
-- and will be held for a long time, you should allocate it at the beginning of
-- a new @ResourceT@ block and then call @resourceForkWith@ from there.
--
-- @since 1.1.9
resourceForkWith
  :: MonadUnliftIO m
  => (IO () -> IO a)
  -> ResourceT m ()
  -> ResourceT m a
resourceForkWith g (ResourceT f) =
  ResourceT $ \r -> withRunInIO $ \run -> E.mask $ \restore ->
    -- We need to make sure the counter is incremented before this call
    -- returns. Otherwise, the parent thread may call runResourceT before
    -- the child thread increments, and all resources will be freed
    -- before the child gets called.
    bracket_
        (stateAlloc r)
        (return ())
        (const $ return ())
        (g $ bracket_
            (return ())
            (stateCleanup ReleaseNormal r)
            (\e -> stateCleanup (ReleaseExceptionWith e) r)
            (restore $ run $ f r))

-- | Launch a new reference counted resource context using @forkIO@.
--
-- This is defined as @resourceForkWith forkIO@.
--
-- Note: Using regular 'forkIO' inside of a 'ResourceT' is inherently unsafe,
-- since the forked thread may try access the resources of the parent after they are cleaned up.
-- When you use 'resourceForkIO' or 'resourceForkWith', 'ResourceT' is made aware of the new thread, and will only cleanup resources when all threads finish.
-- Other concurrency mechanisms, like 'concurrently' or 'race', are safe to use.
--
-- If you encounter 'InvalidAccess' exceptions ("The mutable state is being accessed after cleanup"),
-- use of 'forkIO' is a possible culprit.
--
-- @since 0.3.0
resourceForkIO :: MonadUnliftIO m => ResourceT m () -> ResourceT m ThreadId
resourceForkIO = resourceForkWith forkIO

-- | Just use 'MonadUnliftIO' directly now, legacy explanation continues:
--
-- A @Monad@ which can be used as a base for a @ResourceT@.
--
-- A @ResourceT@ has some restrictions on its base monad:
--
-- * @runResourceT@ requires an instance of @MonadUnliftIO@.
-- * @MonadResource@ requires an instance of @MonadIO@
--
-- Note that earlier versions of @conduit@ had a typeclass @ResourceIO@. This
-- fulfills much the same role.
--
-- Since 0.3.2
type MonadResourceBase = MonadUnliftIO
{-# DEPRECATED MonadResourceBase "Use MonadUnliftIO directly instead" #-}

-- $internalState
--
-- A @ResourceT@ internally is a modified @ReaderT@ monad transformer holding
-- onto a mutable reference to all of the release actions still remaining to be
-- performed. If you are building up a custom application monad, it may be more
-- efficient to embed this @ReaderT@ functionality directly in your own monad
-- instead of wrapping around @ResourceT@ itself. This section provides you the
-- means of doing so.

-- | Create a new internal state. This state must be closed with
-- @closeInternalState@. It is your responsibility to ensure exception safety.
-- Caveat emptor!
--
-- Since 0.4.9
createInternalState :: MonadIO m => m InternalState
createInternalState = liftIO
                    $ I.newIORef
                    $ ReleaseMap maxBound (minBound + 1) IntMap.empty

-- | Close an internal state created by @createInternalState@.
--
-- Since 0.4.9
closeInternalState :: MonadIO m => InternalState -> m ()
closeInternalState = liftIO . stateCleanup ReleaseNormal

-- | Get the internal state of the current @ResourceT@.
--
-- Since 0.4.6
getInternalState :: Monad m => ResourceT m InternalState
getInternalState = ResourceT return

-- | The internal state held by a @ResourceT@ transformer.
--
-- Since 0.4.6
type InternalState = I.IORef ReleaseMap

-- | Unwrap a @ResourceT@ using the given @InternalState@.
--
-- Since 0.4.6
runInternalState :: ResourceT m a -> InternalState -> m a
runInternalState = unResourceT

-- | Run an action in the underlying monad, providing it the @InternalState@.
--
-- Since 0.4.6
withInternalState :: (InternalState -> m a) -> ResourceT m a
withInternalState = ResourceT

-- BEGIN NGLess/Control/Monad/Writer/Class.hs


-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement).
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    {-# MINIMAL (writer | tell), listen, pass #-}
    -- | @'writer' (a,w)@ embeds a simple writer action.
    writer :: (a,w) -> m a
    writer ~(a, w) = do
      tell w
      return a

    -- | @'tell' w@ is an action that produces the output @w@.
    tell   :: w -> m ()
    tell w = writer ((),w)

    -- | @'listen' m@ is an action that executes the action @m@ and adds
    -- its output to the value of the computation.
    listen :: m a -> m (a, w)
    -- | @'pass' m@ is an action that executes the action @m@, which
    -- returns a value and a function, and returns the value, applying
    -- the function to the output.
    pass   :: m (a, w -> w) -> m a

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\x -> (x,f)) m)@
censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

-- | @since 2.2.2
instance (Monoid w) => MonadWriter w ((,) w) where
  writer ~(a, w) = (w, a)
  tell w = (w, ())
  listen ~(w, a) = (w, (a, w))
  pass ~(w, (a, f)) = (f w, a)

-- | @since 2.3
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

-- | @since 2.3
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

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers
--
-- All of these instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.

-- | @since 2.2
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

-- | There are two valid instances for 'AccumT'. It could either:
--
--   1. Lift the operations to the inner @MonadWriter@
--   2. Handle the operations itself, à la a @WriterT@.
--
--   This instance chooses (1), reflecting that the intent
--   of 'AccumT' as a type is different than that of @WriterT@.
--
-- @since 2.3
instance
  ( Monoid w'
  , MonadWriter w m
  ) => MonadWriter w (AccumT w' m) where
    writer = lift . writer
    tell   = lift . tell
    listen = Accum.liftListen listen
    pass   = Accum.liftPass pass

-- BEGIN NGLess/Data/Conduit.hs


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

-- | Core datatype of the conduit package. This type represents a general
-- component which can consume a stream of input values @i@, produce a stream
-- of output values @o@, perform actions in the @m@ monad, and produce a final
-- result @r@. The type synonyms provided here are simply wrappers around this
-- type.
--
-- Since 1.3.0
newtype ConduitT i o m r = ConduitT
    { unConduitT :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

-- | In order to provide for efficient monadic composition, the
-- @ConduitT@ type is implemented internally using a technique known
-- as the codensity transform. This allows for cheap appending, but
-- makes one case much more expensive: partially running a @ConduitT@
-- and that capturing the new state.
--
-- This data type is the same as @ConduitT@, but does not use the
-- codensity transform technique.
--
-- @since 1.3.0
newtype SealedConduitT i o m r = SealedConduitT (Pipe i i o () m r)

-- | Same as 'ConduitT', for backwards compat
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

-- | @since 1.3.1
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

-- | Provides a stream of output values, without consuming any input or
-- producing a final result.
--
-- Since 0.5.0
type Source m o = ConduitT () o m ()
{-# DEPRECATED Source "Use ConduitT directly" #-}

-- | A component which produces a stream of output values, regardless of the
-- input stream. A @Producer@ is a generalization of a @Source@, and can be
-- used as either a @Source@ or a @Conduit@.
--
-- Since 1.0.0
type Producer m o = forall i. ConduitT i o m ()
{-# DEPRECATED Producer "Use ConduitT directly" #-}

-- | Consumes a stream of input values and produces a final result, without
-- producing any output.
--
-- > type Sink i m r = ConduitT i Void m r
--
-- Since 0.5.0
type Sink i = ConduitT i Void
{-# DEPRECATED Sink "Use ConduitT directly" #-}

-- | A component which consumes a stream of input values and produces a final
-- result, regardless of the output stream. A @Consumer@ is a generalization of
-- a @Sink@, and can be used as either a @Sink@ or a @Conduit@.
--
-- Since 1.0.0
type Consumer i m r = forall o. ConduitT i o m r
{-# DEPRECATED Consumer "Use ConduitT directly" #-}

-- | Consumes a stream of input values and produces a stream of output values,
-- without producing a final result.
--
-- Since 0.5.0
type Conduit i m o = ConduitT i o m ()
{-# DEPRECATED Conduit "Use ConduitT directly" #-}

sealConduitT :: ConduitT i o m r -> SealedConduitT i o m r
sealConduitT (ConduitT f) = SealedConduitT (f Done)

unsealConduitT :: Monad m => SealedConduitT i o m r -> ConduitT i o m r
unsealConduitT (SealedConduitT f) = ConduitT (f >>=)

-- | Connect a @Source@ to a @Sink@ until the latter closes. Returns both the
-- most recent state of the @Source@ and the result of the @Sink@.
--
-- Since 0.5.0
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

-- | Generalize a 'Source' to a 'Producer'.
--
-- Since 1.0.0
toProducer :: Monad m => ConduitT () a m () -> ConduitT i a m ()
toProducer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput _ c) = go (c ())
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p ()) = go p
    in go (c0 Done)

-- | Generalize a 'Sink' to a 'Consumer'.
--
-- Since 1.0.0
toConsumer :: Monad m => ConduitT a Void m b -> ConduitT a o m b
toConsumer (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput _ o) = absurd o
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover p l) = Leftover (go p) l
    in go (c0 Done)

-- | Catch all exceptions thrown by the current component of the pipeline.
--
-- Note: this will /not/ catch exceptions thrown by other components! For
-- example, if an exception is thrown in a @Source@ feeding to a @Sink@, and
-- the @Sink@ uses @catchC@, the exception will /not/ be caught.
--
-- Due to this behavior (as well as lack of async exception safety), you
-- should not try to implement combinators such as @onException@ in terms of this
-- primitive function.
--
-- Note also that the exception handling will /not/ be applied to any
-- finalizers generated by this conduit.
--
-- Since 1.0.11
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

-- | The same as @flip catchC@.
--
-- Since 1.0.11
handleC :: (MonadUnliftIO m, Exception e)
        => (e -> ConduitT i o m r)
        -> ConduitT i o m r
        -> ConduitT i o m r
handleC = flip catchC
{-# INLINE handleC #-}

-- | A version of @try@ for use within a pipeline. See the comments in @catchC@
-- for more details.
--
-- Since 1.0.11
tryC :: (MonadUnliftIO m, Exception e)
     => ConduitT i o m r
     -> ConduitT i o m (Either e r)
tryC c = fmap Right c `catchC` (return . Left)
{-# INLINE tryC #-}

-- | Combines two sinks. The new sink will complete when both input sinks have
--   completed.
--
-- Any leftovers are discarded.
--
-- Since 0.4.1
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

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
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

-- | Combines two sources. The new source will stop producing once either
--   source has been exhausted.
--
-- Since 1.0.13
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

-- |
--
-- Since 1.0.17
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

-- | Same as normal fusion (e.g. @=$=@), except instead of discarding leftovers
-- from the downstream component, return them.
--
-- Since 1.0.17
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

-- | Similar to @fuseReturnLeftovers@, but use the provided function to convert
-- downstream leftovers to upstream leftovers.
--
-- Since 1.0.17
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

-- | Connect a 'Conduit' to a sink and return the output of the sink
-- together with a new 'Conduit'.
--
-- Since 1.0.17
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

-- | Merge a @Source@ into a @Conduit@.
-- The new conduit will stop processing once either source or upstream have been exhausted.
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


-- | Turn a @Sink@ into a @Conduit@ in the following way:
--
-- * All input passed to the @Sink@ is yielded downstream.
--
-- * When the @Sink@ finishes processing, the result is passed to the provided to the finalizer function.
--
-- Note that the @Sink@ will stop receiving input as soon as the downstream it
-- is connected to shuts down.
--
-- An example usage would be to write the result of a @Sink@ to some mutable
-- variable while allowing other processing to continue.
--
-- Since 1.1.0
passthroughSink :: Monad m
                => ConduitT i Void m r
                -> (r -> m ()) -- ^ finalizer
                -> ConduitT i i m ()
passthroughSink (ConduitT sink0) final = ConduitT $ \rest -> let
    -- A bit of explanation is in order, this function is
    -- non-obvious. The purpose of go is to keep track of the sink
    -- we're passing values to, and then yield values downstream. The
    -- third argument to go is the current state of that sink. That's
    -- relatively straightforward.
    --
    -- The second value is the leftover buffer. These are values that
    -- the sink itself has called leftover on, and must be provided
    -- back to the sink the next time it awaits. _However_, these
    -- values should _not_ be reyielded downstream: we have already
    -- yielded them downstream ourself, and it is the responsibility
    -- of the functions wrapping around passthroughSink to handle the
    -- leftovers from downstream.
    --
    -- The trickiest bit is the first argument, which is a solution to
    -- bug https://github.com/snoyberg/conduit/issues/304. The issue
    -- is that, once we get a value, we need to provide it to both the
    -- inner sink _and_ yield it downstream. The obvious thing to do
    -- is yield first and then recursively call go. Unfortunately,
    -- this doesn't work in all cases: if the downstream component
    -- never calls await again, our yield call will never return, and
    -- our sink will not get the last value. This results is confusing
    -- behavior where the sink and downstream component receive a
    -- different number of values.
    --
    -- Solution: keep a buffer of the next value to yield downstream,
    -- and only yield it downstream in one of two cases: our sink is
    -- asking for another value, or our sink is done. This way, we
    -- ensure that, in all cases, we pass exactly the same number of
    -- values to the inner sink as to downstream.

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

-- | Convert a @Source@ into a list. The basic functionality can be explained as:
--
-- > sourceToList src = src $$ Data.Conduit.List.consume
--
-- However, @sourceToList@ is able to produce its results lazily, which cannot
-- be done when running a conduit pipeline in general. Unlike the
-- @Data.Conduit.Lazy@ module (in conduit-extra), this function performs no
-- unsafe I\/O operations, and therefore can only be as lazy as the
-- underlying monad.
--
-- Since 1.2.6
sourceToList :: Monad m => ConduitT () a m () -> m [a]
sourceToList (ConduitT k) =
    go $ k Done
  where
    go (Done _) = return []
    go (HaveOutput src x) = liftM (x:) (go src)
    go (PipeM msrc) = msrc >>= go
    go (NeedInput _ c) = go (c ())
    go (Leftover p _) = go p

-- Define fixity of all our operators
infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=
infixr 0 $$+
infixr 0 $$++
infixr 0 $$+-
infixl 1 $=+
infixr 2 .|

-- | Equivalent to using 'runConduit' and '.|' together.
--
-- Since 1.2.3
connect :: Monad m
        => ConduitT () a m ()
        -> ConduitT a Void m r
        -> m r
connect = ($$)

-- | Split a conduit into head and tail.
--
-- Note that you have to 'sealConduitT' it first.
--
-- Since 1.3.3
unconsM :: Monad m
        => SealedConduitT () o m ()
        -> m (Maybe (o, SealedConduitT () o m ()))
unconsM (SealedConduitT p) = go p
  where
    -- This function is the same as @Pipe.unconsM@ but it ignores leftovers.
    go (HaveOutput p o) = pure $ Just (o, SealedConduitT p)
    go (NeedInput _ c) = go $ c ()
    go (Done ()) = pure Nothing
    go (PipeM mp) = mp >>= go
    go (Leftover p ()) = go p

-- | Split a conduit into head and tail or return its result if it is done.
--
-- Note that you have to 'sealConduitT' it first.
--
-- Since 1.3.3
unconsEitherM :: Monad m
              => SealedConduitT () o m r
              -> m (Either r (o, SealedConduitT () o m r))
unconsEitherM (SealedConduitT p) = go p
  where
    -- This function is the same as @Pipe.unconsEitherM@ but it ignores leftovers.
    go (HaveOutput p o) = pure $ Right (o, SealedConduitT p)
    go (NeedInput _ c) = go $ c ()
    go (Done r) = pure $ Left r
    go (PipeM mp) = mp >>= go
    go (Leftover p ()) = go p

-- | Named function synonym for '.|'
--
-- Equivalent to '.|' and '=$='. However, the latter is
-- deprecated and will be removed in a future version.
--
-- Since 1.2.3
fuse :: Monad m => ConduitT a b m () -> ConduitT b c m r -> ConduitT a c m r
fuse = (=$=)

-- | Combine two @Conduit@s together into a new @Conduit@ (aka 'fuse').
--
-- Output from the upstream (left) conduit will be fed into the
-- downstream (right) conduit. Processing will terminate when
-- downstream (right) returns.
-- Leftover data returned from the right @Conduit@ will be discarded.
--
-- Equivalent to 'fuse' and '=$=', however the latter is deprecated and will
-- be removed in a future version.
--
-- Note that, while this operator looks like categorical composition
-- (from "Control.Category"), there are a few reasons it's different:
--
-- * The position of the type parameters to 'ConduitT' do not
--   match. We would need to change @ConduitT i o m r@ to @ConduitT r
--   m i o@, which would preclude a 'Monad' or 'MonadTrans' instance.
--
-- * The result value from upstream and downstream are allowed to
--   differ between upstream and downstream. In other words, we would
--   need the type signature here to look like @ConduitT a b m r ->
--   ConduitT b c m r -> ConduitT a c m r@.
--
-- * Due to leftovers, we do not have a left identity in Conduit. This
--   can be achieved with the underlying @Pipe@ datatype, but this is
--   not generally recommended. See <https://stackoverflow.com/a/15263700>.
--
-- @since 1.2.8
(.|) :: Monad m
     => ConduitT a b m () -- ^ upstream
     -> ConduitT b c m r -- ^ downstream
     -> ConduitT a c m r
(.|) = fuse
{-# INLINE (.|) #-}

-- | The connect operator, which pulls data from a source and pushes to a sink.
-- If you would like to keep the @Source@ open to be used for other
-- operations, use the connect-and-resume operator '$$+'.
--
-- Since 0.4.0
($$) :: Monad m => Source m a -> Sink a m b -> m b
src $$ sink = do
    (rsrc, res) <- src $$+ sink
    rsrc $$+- return ()
    return res
{-# INLINE [1] ($$) #-}
{-# DEPRECATED ($$) "Use runConduit and .|" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
($=) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
($=) = (=$=)
{-# INLINE [0] ($=) #-}
{-# RULES "conduit: $= is =$=" ($=) = (=$=) #-}
{-# DEPRECATED ($=) "Use .|" #-}

-- | A synonym for '=$=' for backwards compatibility.
--
-- Since 0.4.0
(=$) :: Monad m => Conduit a m b -> ConduitT b c m r -> ConduitT a c m r
(=$) = (=$=)
{-# INLINE [0] (=$) #-}
{-# RULES "conduit: =$ is =$=" (=$) = (=$=) #-}
{-# DEPRECATED (=$) "Use .|" #-}

-- | Deprecated fusion operator.
--
-- Since 0.4.0
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

-- | Wait for a single input value from upstream. If no data is available,
-- returns @Nothing@. Once @await@ returns @Nothing@, subsequent calls will
-- also return @Nothing@.
--
-- Since 0.5.0
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

-- | Send a value downstream to the next component to consume. If the
-- downstream component terminates, this call will never return control.
--
-- Since 0.5.0
yield :: Monad m
      => o -- ^ output value
      -> ConduitT i o m ()
yield o = ConduitT $ \rest -> HaveOutput (rest ()) o
{-# INLINE yield #-}

-- | Send a monadic value downstream for the next component to consume.
--
-- @since 1.2.7
yieldM :: Monad m => m o -> ConduitT i o m ()
yieldM mo = lift mo >>= yield
{-# INLINE yieldM #-}

  -- FIXME rule won't fire, see FIXME in .Pipe; "mapM_ yield" mapM_ yield = ConduitT . sourceList

-- | Provide a single piece of leftover input to be consumed by the next
-- component in the current monadic binding.
--
-- /Note/: it is highly encouraged to only return leftover values from input
-- already consumed from upstream.
--
-- @since 0.5.0
leftover :: i -> ConduitT i o m ()
leftover i = ConduitT $ \rest -> Leftover (rest ()) i
{-# INLINE leftover #-}

-- | Run a pipeline until processing completes.
--
-- Since 1.2.1
runConduit :: Monad m => ConduitT () Void m r -> m r
runConduit (ConduitT p) = runPipe $ injectLeftovers $ p Done
{-# INLINE [0] runConduit #-}

-- | Bracket a conduit computation between allocation and release of a
-- resource. Two guarantees are given about resource finalization:
--
-- 1. It will be /prompt/. The finalization will be run as early as possible.
--
-- 2. It is exception safe. Due to usage of @resourcet@, the finalization will
-- be run in the event of any exceptions.
--
-- Since 0.5.0
bracketP :: MonadResource m

         => IO a
            -- ^ computation to run first (\"acquire resource\")
         -> (a -> IO ())
            -- ^ computation to run last (\"release resource\")
         -> (a -> ConduitT i o m r)
            -- ^ computation to run in-between
         -> ConduitT i o m r
            -- returns the value from the in-between computation
bracketP alloc free inside = ConduitT $ \rest -> do
  (key, seed) <- allocate alloc free
  unConduitT (inside seed) $ \res -> do
    release key
    rest res

-- | Wait for input forever, calling the given inner component for each piece of
-- new input.
--
-- This function is provided as a convenience for the common pattern of
-- @await@ing input, checking if it's @Just@ and then looping.
--
-- Since 0.5.0
awaitForever :: Monad m => (i -> ConduitT i o m r) -> ConduitT i o m ()
awaitForever f = ConduitT $ \rest ->
    let go = NeedInput (\i -> unConduitT (f i) (const go)) rest
     in go

-- | Transform the monad that a @ConduitT@ lives in.
--
-- Note that the monad transforming function will be run multiple times,
-- resulting in unintuitive behavior in some cases. For a fuller treatment,
-- please see:
--
-- <https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers>
--
-- Since 0.4.0
transPipe :: Monad m => (forall a. m a -> n a) -> ConduitT i o m r -> ConduitT i o n r
transPipe f (ConduitT c0) = ConduitT $ \rest -> let
        go (HaveOutput p o) = HaveOutput (go p) o
        go (NeedInput p c) = NeedInput (go . p) (go . c)
        go (Done r) = rest r
        go (PipeM mp) =
            PipeM (f $ liftM go $ collapse mp)
          where
            -- Combine a series of monadic actions into a single action.  Since we
            -- throw away side effects between different actions, an arbitrary break
            -- between actions will lead to a violation of the monad transformer laws.
            -- Example available at:
            --
            -- http://hpaste.org/75520
            collapse mpipe = do
                pipe' <- mpipe
                case pipe' of
                    PipeM mpipe' -> collapse mpipe'
                    _ -> return pipe'
        go (Leftover p i) = Leftover (go p) i
        in go (c0 Done)

-- | Apply a function to all the output values of a @ConduitT@.
--
-- This mimics the behavior of `fmap` for a `Source` and `Conduit` in pre-0.4
-- days. It can also be simulated by fusing with the @map@ conduit from
-- "Data.Conduit.List".
--
-- Since 0.4.1
mapOutput :: Monad m => (o1 -> o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutput f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) (f o)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

-- | Same as 'mapOutput', but use a function that returns @Maybe@ values.
--
-- Since 0.5.0
mapOutputMaybe :: Monad m => (o1 -> Maybe o2) -> ConduitT i o1 m r -> ConduitT i o2 m r
mapOutputMaybe f (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = maybe id (\o' p' -> HaveOutput p' o') (f o) (go p)
    go (NeedInput p c) = NeedInput (go . p) (go . c)
    go (Done r) = rest r
    go (PipeM mp) = PipeM (liftM (go) mp)
    go (Leftover p i) = Leftover (go p) i
    in go (c0 Done)

-- | Apply a function to all the input values of a @ConduitT@.
--
-- Since 0.5.0
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

-- | Apply a monadic action to all the input values of a @ConduitT@.
--
-- Since 1.3.2
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

-- | The connect-and-resume operator. This does not close the @Source@, but
-- instead returns it to be used again. This allows a @Source@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Mnemonic: connect + do more.
--
-- Since 0.5.0
($$+) :: Monad m => ConduitT () a m () -> ConduitT a Void m b -> m (SealedConduitT () a m (), b)
src $$+ sink = connectResume (sealConduitT src) sink
{-# INLINE ($$+) #-}

-- | Continue processing after usage of @$$+@.
--
-- Since 0.5.0
($$++) :: Monad m => SealedConduitT () a m () -> ConduitT a Void m b -> m (SealedConduitT () a m (), b)
($$++) = connectResume
{-# INLINE ($$++) #-}

-- | Same as @$$++@ and @connectResume@, but doesn't include the
-- updated @SealedConduitT@.
--
-- /NOTE/ In previous versions, this would cause finalizers to
-- run. Since version 1.3.0, there are no finalizers in conduit.
--
-- Since 0.5.0
($$+-) :: Monad m => SealedConduitT () a m () -> ConduitT a Void m b -> m b
rsrc $$+- sink = do
    (_, res) <- connectResume rsrc sink
    return res
{-# INLINE ($$+-) #-}

-- | Left fusion for a sealed source.
--
-- Since 1.0.16
($=+) :: Monad m => SealedConduitT () a m () -> ConduitT a b m () -> SealedConduitT () b m ()
SealedConduitT src $=+ ConduitT sink = SealedConduitT (src `pipeL` sink Done)

-- | Provide for a stream of data that can be flushed.
--
-- A number of @Conduit@s (e.g., zlib compression) need the ability to flush
-- the stream at some point. This provides a single wrapper datatype to be used
-- in all such circumstances.
--
-- Since 0.3.0
data Flush a = Chunk a | Flush
    deriving (Show, Eq, Ord)
instance Functor Flush where
    fmap _ Flush = Flush
    fmap f (Chunk a) = Chunk (f a)

-- | A wrapper for defining an 'Applicative' instance for 'Source's which allows
-- to combine sources together, generalizing 'zipSources'. A combined source
-- will take input yielded from each of its @Source@s until any of them stop
-- producing output.
--
-- Since 1.0.13
newtype ZipSource m o = ZipSource { getZipSource :: ConduitT () o m () }

instance Monad m => Functor (ZipSource m) where
    fmap f = ZipSource . mapOutput f . getZipSource
instance Monad m => Applicative (ZipSource m) where
    pure  = ZipSource . forever . yield
    (ZipSource f) <*> (ZipSource x) = ZipSource $ zipSourcesApp f x

-- | Coalesce all values yielded by all of the @Source@s.
--
-- Implemented on top of @ZipSource@ and as such, it exhibits the same
-- short-circuiting behavior as @ZipSource@. See that data type for more
-- details. If you want to create a source that yields *all* values from
-- multiple sources, use `sequence_`.
--
-- Since 1.0.13
sequenceSources :: (Traversable f, Monad m) => f (ConduitT () o m ()) -> ConduitT () (f o) m ()
sequenceSources = getZipSource . sequenceA . fmap ZipSource

-- | A wrapper for defining an 'Applicative' instance for 'Sink's which allows
-- to combine sinks together, generalizing 'zipSinks'. A combined sink
-- distributes the input to all its participants and when all finish, produces
-- the result. This allows to define functions like
--
-- @
-- sequenceSinks :: (Monad m)
--           => [ConduitT i Void m r] -> ConduitT i Void m [r]
-- sequenceSinks = getZipSink . sequenceA . fmap ZipSink
-- @
--
-- Note that the standard 'Applicative' instance for conduits works
-- differently. It feeds one sink with input until it finishes, then switches
-- to another, etc., and at the end combines their results.
--
-- This newtype is in fact a type constrained version of 'ZipConduit', and has
-- the same behavior. It's presented as a separate type since (1) it
-- historically predates @ZipConduit@, and (2) the type constraining can make
-- your code clearer (and thereby make your error messages more easily
-- understood).
--
-- Since 1.0.13
newtype ZipSink i m r = ZipSink { getZipSink :: ConduitT i Void m r }

instance Monad m => Functor (ZipSink i m) where
    fmap f (ZipSink x) = ZipSink (liftM f x)
instance Monad m => Applicative (ZipSink i m) where
    pure  = ZipSink . return
    (ZipSink f) <*> (ZipSink x) =
         ZipSink $ liftM (uncurry ($)) $ zipSinks f x

-- | Send incoming values to all of the @Sink@ providing, and ultimately
-- coalesce together all return values.
--
-- Implemented on top of @ZipSink@, see that data type for more details.
--
-- Since 1.0.13
sequenceSinks :: (Traversable f, Monad m) => f (ConduitT i Void m r) -> ConduitT i Void m (f r)
sequenceSinks = getZipSink . sequenceA . fmap ZipSink

-- | The connect-and-resume operator. This does not close the @Conduit@, but
-- instead returns it to be used again. This allows a @Conduit@ to be used
-- incrementally in a large program, without forcing the entire program to live
-- in the @Sink@ monad.
--
-- Leftover data returned from the @Sink@ will be discarded.
--
-- Mnemonic: connect + do more.
--
-- Since 1.0.17
(=$$+) :: Monad m
       => ConduitT a b m ()
       -> ConduitT b Void m r
       -> ConduitT a Void m (SealedConduitT a b m (), r)
(=$$+) conduit = connectResumeConduit (sealConduitT conduit)
{-# INLINE (=$$+) #-}

-- | Continue processing after usage of '=$$+'. Connect a 'SealedConduitT' to
-- a sink and return the output of the sink together with a new
-- 'SealedConduitT'.
--
-- Since 1.0.17
(=$$++) :: Monad m => SealedConduitT i o m () -> ConduitT o Void m r -> ConduitT i Void m (SealedConduitT i o m (), r)
(=$$++) = connectResumeConduit
{-# INLINE (=$$++) #-}

-- | Same as @=$$++@, but doesn't include the updated
-- @SealedConduitT@.
--
-- /NOTE/ In previous versions, this would cause finalizers to
-- run. Since version 1.3.0, there are no finalizers in conduit.
--
-- Since 1.0.17
(=$$+-) :: Monad m => SealedConduitT i o m () -> ConduitT o Void m r -> ConduitT i Void m r
rsrc =$$+- sink = do
    (_, res) <- connectResumeConduit rsrc sink
    return res
{-# INLINE (=$$+-) #-}


infixr 0 =$$+
infixr 0 =$$++
infixr 0 =$$+-

-- | Provides an alternative @Applicative@ instance for @ConduitT@. In this instance,
-- every incoming value is provided to all @ConduitT@s, and output is coalesced together.
-- Leftovers from individual @ConduitT@s will be used within that component, and then discarded
-- at the end of their computation. Output and finalizers will both be handled in a left-biased manner.
--
-- As an example, take the following program:
--
-- @
-- main :: IO ()
-- main = do
--     let src = mapM_ yield [1..3 :: Int]
--         conduit1 = CL.map (+1)
--         conduit2 = CL.concatMap (replicate 2)
--         conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
--         sink = CL.mapM_ print
--     src $$ conduit =$ sink
-- @
--
-- It will produce the output: 2, 1, 1, 3, 2, 2, 4, 3, 3
--
-- Since 1.0.17
newtype ZipConduit i o m r = ZipConduit { getZipConduit :: ConduitT i o m r }
    deriving Functor
instance Monad m => Applicative (ZipConduit i o m) where
    pure = ZipConduit . pure
    ZipConduit left <*> ZipConduit right = ZipConduit (zipConduitApp left right)

-- | Provide identical input to all of the @Conduit@s and combine their outputs
-- into a single stream.
--
-- Implemented on top of @ZipConduit@, see that data type for more details.
--
-- Since 1.0.17
sequenceConduits :: (Traversable f, Monad m) => f (ConduitT i o m r) -> ConduitT i o m (f r)
sequenceConduits = getZipConduit . sequenceA . fmap ZipConduit

-- | Fuse two @ConduitT@s together, and provide the return value of both. Note
-- that this will force the entire upstream @ConduitT@ to be run to produce the
-- result value, even if the downstream terminates early.
--
-- Since 1.1.5
fuseBoth :: Monad m => ConduitT a b m r1 -> ConduitT b c m r2 -> ConduitT a c m (r1, r2)
fuseBoth (ConduitT up) (ConduitT down) =
    ConduitT (pipeL (up Done) (withUpstream $ generalizeUpstream $ down Done) >>=)
{-# INLINE fuseBoth #-}

-- | Like 'fuseBoth', but does not force consumption of the @Producer@.
-- In the case that the @Producer@ terminates, the result value is
-- provided as a @Just@ value. If it does not terminate, then a
-- @Nothing@ value is returned.
--
-- One thing to note here is that "termination" here only occurs if the
-- @Producer@ actually yields a @Nothing@ value. For example, with the
-- @Producer@ @mapM_ yield [1..5]@, if five values are requested, the
-- @Producer@ has not yet terminated. Termination only occurs when the
-- sixth value is awaited for and the @Producer@ signals termination.
--
-- Since 1.2.4
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

-- | Same as @fuseBoth@, but ignore the return value from the downstream
-- @Conduit@. Same caveats of forced consumption apply.
--
-- Since 1.1.5
fuseUpstream :: Monad m => ConduitT a b m r -> ConduitT b c m () -> ConduitT a c m r
fuseUpstream up down = fmap fst (fuseBoth up down)
{-# INLINE fuseUpstream #-}

-- Rewrite rules

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

-- | Run a pure pipeline until processing completes, i.e. a pipeline
-- with @Identity@ as the base monad. This is equivalient to
-- @runIdentity . runConduit@.
--
-- @since 1.2.8
runConduitPure :: ConduitT () Void Identity r -> r
runConduitPure = runIdentity . runConduit
{-# INLINE runConduitPure #-}

-- | Run a pipeline which acquires resources with @ResourceT@, and
-- then run the @ResourceT@ transformer. This is equivalent to
-- @runResourceT . runConduit@.
--
-- @since 1.2.8
runConduitRes :: MonadUnliftIO m
              => ConduitT () Void (ResourceT m) r
              -> m r
runConduitRes = runResourceT . runConduit
{-# INLINE runConduitRes #-}

-- BEGIN NGLess/Data/Conduit/Binary.hs


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

-- BEGIN NGLess/Data/Conduit/Combinators.hs


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
