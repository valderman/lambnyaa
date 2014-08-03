{-# LANGUAGE FlexibleInstances, UndecidableInstances, TupleSections #-}
module Network.LambNyaa.Monad (
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.IO.Class,
    Nyaa, NyaaEnv (..), runNyaa, getEnv, source, accept
  ) where
import Data.Monoid
import Data.Hashable
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Database.SQLite.Simple as DB
import Network.LambNyaa.Database
import Network.LambNyaa.Config
import Network.LambNyaa.Sink
import Network.LambNyaa.Item

data NyaaEnv = NyaaEnv {
    envConn   :: DB.Connection,
    envConfig :: Config
  }

newtype Nyaa a = Nyaa {unNyaa :: NyaaEnv -> IO (S.Set Item, a)}

instance Functor Nyaa where
  fmap f (Nyaa n) = Nyaa $ \cfg -> do
    (s, n') <- n cfg
    return (s, f n')

instance Applicative Nyaa where
  pure x = Nyaa . const $ return (S.empty, x)
  (Nyaa f) <*> (Nyaa x) = Nyaa $ \cfg -> do
    (s1, f') <- f cfg
    (s2, x') <- x cfg
    return (S.union s1 s2, f' x')

instance Monad Nyaa where
  return = pure
  Nyaa m >>= f = Nyaa $ \cfg -> do
    (s1, x) <- m cfg
    (s2, x') <- unNyaa (f x) cfg
    return (S.union s1 s2, x')

class SinkLike s where
  accept :: ItemLike a => s -> [a] -> Nyaa ()

instance SinkLike Sink where
  accept s xs = Nyaa $ \cfg -> do
      unSink s (envConfig cfg) xs'
      return (S.fromList xs', ())
    where
      xs' = map toItem xs

instance SinkLike [Sink] where
  accept ss xs = Nyaa $ \cfg -> do
      forM_ ss $ \s -> unSink s (envConfig cfg) xs'
      return (S.fromList xs', ())
    where
      xs' = map toItem xs

instance Monoid a => Monoid (Nyaa a) where
  mappend = liftM2 mappend
  mempty = return mempty

instance MonadIO Nyaa where
  liftIO = Nyaa . const . fmap (S.empty, )

-- | Get the environment the computation is executing in.
getEnv :: Nyaa NyaaEnv
getEnv = Nyaa $ \env -> return (S.empty, env)

-- | Execute a LambNyaa computation, returning the number of distinct items
--   accepted into some sink.
runNyaa :: NyaaEnv -> Nyaa () -> IO Int
runNyaa cfg (Nyaa nyaa) = do
  (s, _) <- nyaa cfg
  return (S.size s)

-- | Use an action returning a list of item-ish values as a source.
--   Unlike simply lifting such an IO computation into the Nyaa monad, using
--   @source@ will ensure that the item's @itmIdentifier@ field is properly
--   filled in.
source :: ItemLike a => IO [a] -> Nyaa [a]
source m = do
  env <- getEnv
  xs <- liftIO m
  liftIO $ mapM (asItemM (fillFields $ envConn env)) xs

-- | Fill in the itmSeenBefore and itmIdentifier fields of an Item.
fillFields :: DB.Connection -> Item -> IO Item
fillFields c item = do
    seen <- wasSeen ident c
    return $ item {itmSeenBefore = seen, itmIdentifier = ident}
  where
    ident = hash item
