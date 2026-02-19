{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Allocate resources which are guaranteed to be released.
--
-- For more information, see <https://github.com/snoyberg/conduit/tree/master/resourcet#readme>.
--
-- One point to note: all register cleanup actions live in the @IO@ monad, not
-- the main monad. This allows both more efficient code, and for monads to be
-- transformed.
module Control.Monad.Trans.Resource
    ( -- * Data types
      ResourceT
    , ResIO
    , ReleaseKey
      -- * Unwrap
    , runResourceT
      -- ** Check cleanup exceptions
    , runResourceTChecked
    , ResourceCleanupException (..)
      -- * Special actions
    , resourceForkWith
    , resourceForkIO
      -- * Monad transformation
    , transResourceT
    , joinResourceT
      -- * Registering/releasing
    , allocate
    , allocate_
    , register
    , release
    , unprotect
    , resourceMask
      -- * Type class/associated types
    , MonadResource (..)
    , MonadResourceBase
      -- ** Low-level
    , InvalidAccess (..)
      -- * Re-exports
    , MonadUnliftIO
      -- * Internal state
      -- $internalState
    , InternalState
    , getInternalState
    , runInternalState
    , withInternalState
    , createInternalState
    , closeInternalState
      -- * Reexport
    , MonadThrow (..)
    ) where
import Control.Exception (throw, Exception, SomeException)
import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Cont.Class (MonadCont (..))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Writer.Class (MonadWriter (..))
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
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Catch (MonadThrow (..), MonadCatch (..), MonadMask (..))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IORef as I
import Data.Typeable
import Data.Word (Word)
import qualified Control.Exception as E
import Control.Concurrent (ThreadId, forkIO)

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
