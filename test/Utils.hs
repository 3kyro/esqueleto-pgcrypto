{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (createPostgresqlPool)
import Model
import Test.Hspec
import UnliftIO

mkConnectionPool :: IO ConnectionPool
mkConnectionPool = do
    pool <-
        runStderrLoggingT $
            createPostgresqlPool
                "host=localhost port=5432 user=pgctest password=pgctest dbname=pgctest"
                4
    flip runSqlPool pool $ do
        migrateIt
    pure pool

migrateIt :: MonadUnliftIO m => SqlPersistT m ()
migrateIt = mapReaderT runNoLoggingT $ do
    void $
        runMigrationSilent $ do
            migrateAll
    cleanDB

cleanDB ::
    forall m.
    (MonadIO m) =>
    SqlPersistT m ()
cleanDB =
    delete $ from (table @User) >> pure ()

type SpecDb = SpecWith ConnectionPool

asserting :: MonadIO f => IO () -> SqlPersistT f ()
asserting = liftIO

itDb ::
    HasCallStack =>
    String ->
    SqlPersistT IO x ->
    SpecDb
itDb message action =
    it message $ \connection -> do
        void $ testDb connection action

testDb :: ConnectionPool -> SqlPersistT IO a -> IO a
testDb conn action =
    liftIO $
        flip runSqlPool conn $ do
            a <- action
            transactionUndo
            pure a
