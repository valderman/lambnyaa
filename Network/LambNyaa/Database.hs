{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | SQLite utilities. Sources and sinks are free to create their own tables
--   in the database. However, care should be taken to choose non-clashing
--   names. In particular, names starting with an underscore are reserved for
--   "official" tables.
module Network.LambNyaa.Database (withSQLite, markSeen, wasSeen) where
import Network.LambNyaa.Config
import Database.SQLite.Simple
import System.Directory
import System.FilePath
import Control.Monad
import Control.Applicative

-- | Perform an action with an SQLite connection.
withSQLite :: Config -> (Connection -> IO a) -> IO a
withSQLite cfg m = do
  let fp = cfgDatabase cfg
  f <- doesFileExist fp
  conn <- open fp
  when (not f) $ initDB conn
  m conn <* close conn

-- | Initialize database. There's only a single "have I seen this before?"
--   table.
initDB :: Connection -> IO ()
initDB c = execute_ c $ "CREATE TABLE _seen (id INTEGER PRIMARY KEY NOT NULL);"

-- | Mark an item as seen or unseen.
markSeen :: Int -> Bool -> Config -> IO ()
markSeen ident True cfg = withSQLite cfg $ \c -> do
  execute c "INSERT INTO _seen VALUES (?)" (Only ident)
markSeen ident _ cfg = withSQLite cfg $ \c -> do
  execute c "DELETE FROM _seen WHERE id = ?" (Only ident)

-- | Was the given item seen before?
wasSeen :: Int -> Connection -> IO Bool
wasSeen ident c = do
  res <- query c "SELECT id FROM _seen WHERE id = ?" (Only ident)
  case res of
    [Only (n :: Int)] -> return True
    _                 -> return False
