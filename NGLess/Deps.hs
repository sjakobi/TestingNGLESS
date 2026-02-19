{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    ( throw, Exception, SomeException )
import Control.Monad ( ap, liftM, MonadPlus(..), unless )
import Control.Monad.Catch
    ( MonadThrow(..), MonadCatch(..), MonadMask(..) )
import Control.Monad.Fail ( MonadFail(..) )
import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Trans.Cont ( ContT )
import Control.Monad.Trans.Except ( ExceptT )
import Control.Monad.Trans.Identity
    ( IdentityT, IdentityT(..) )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Trans.Reader
    ( ReaderT, ReaderT(..) )
import Control.Monad.Trans.State ( StateT )
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy.Internal ( defaultChunkSize )
import Data.IntMap ( IntMap )
import Data.Void ( Void, absurd )
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
      Monoid(mempty),
      Bool,
      String,
      Int,
      Maybe(..),
      type (~),
      Word,
      const,
      (.),
      id,
      flip,
      concat,
      reverse,
      FilePath,
      either,
      maybe )
import qualified Control.Exception as E
    ( try, catch, mask, mask_, throwIO, SomeException )
import qualified Data.IORef as I
    ( atomicModifyIORef, newIORef, IORef )
import qualified System.IO as IO
    ( hClose, IOMode(ReadMode), Handle, openBinaryFile )
import qualified Data.IntMap as IntMap
    ( delete, elems, empty, insert, lookup )
import qualified Control.Monad.ST.Lazy as L
    ( lazyToStrictST, strictToLazyST, ST )
import qualified Data.ByteString as S
    ( concat, drop, elemIndex, hGetSome, null, splitAt, ByteString )

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

instance PrimMonad m => PrimMonad (ExceptT e m) where
  type PrimState (ExceptT e m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

instance PrimBase (ST s) where
  internal (ST p) = p
  {-# INLINE internal #-}

instance PrimMonad (L.ST s) where
  type PrimState (L.ST s) = s
  primitive = L.strictToLazyST . primitive
  {-# INLINE primitive #-}

instance PrimBase (L.ST s) where
  internal = internal . L.lazyToStrictST
  {-# INLINE internal #-}

class (PrimMonad m, s ~ PrimState m) => MonadPrim s m
instance (PrimMonad m, s ~ PrimState m) => MonadPrim s m

class (PrimBase m, MonadPrim s m) => MonadPrimBase s m
instance (PrimBase m, MonadPrim s m) => MonadPrimBase s m

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
  generalBracket acquire cleanup use =
    ResourceT $ \r ->
        generalBracket
            ( unResourceT acquire r )
            ( \resource exitCase ->
                  unResourceT ( cleanup resource exitCase ) r
            )
            ( \resource -> unResourceT ( use resource ) r )
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

instance MonadResource m => MonadResource (IdentityT m) where
  liftResourceT = lift . liftResourceT

instance MonadResource m => MonadResource (MaybeT m) where
  liftResourceT = lift . liftResourceT

instance MonadResource m => MonadResource (ExceptT e m) where
  liftResourceT = lift . liftResourceT

instance MonadResource m => MonadResource (ReaderT r m) where
  liftResourceT = lift . liftResourceT

instance MonadResource m => MonadResource (ContT r m) where
  liftResourceT = lift . liftResourceT

instance MonadResource m => MonadResource (StateT s m) where
  liftResourceT = lift . liftResourceT

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

instance PrimMonad m => PrimMonad (Pipe l i o u m) where
  type PrimState (Pipe l i o u m) = PrimState m
  primitive = lift . primitive

instance MonadResource m => MonadResource (Pipe l i o u m) where
    liftResourceT = lift . liftResourceT
    {-# INLINE liftResourceT #-}

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
