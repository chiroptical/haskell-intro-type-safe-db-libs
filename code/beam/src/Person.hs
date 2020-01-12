{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes    #-}

module Person where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Database.Beam
import           Database.Beam.Backend.SQL      ( HasSqlValueSyntax(..)
                                                , FromBackendRow(..)
                                                )
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite           ( Sqlite
                                                , SqliteM
                                                )
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )
import           Database.Beam.Sqlite.Syntax    ( sqliteTextType )
import           Database.Beam.Schema           ( Columnar )
import           Database.SQLite.Simple         ( Connection )

newtype Username =
  Username
    { _username :: Text
    } deriving (Show, Eq)

instance HasSqlValueSyntax be Text =>
         HasSqlValueSyntax be Username where
  sqlValueSyntax = sqlValueSyntax .
    (_username :: Username -> Text)

instance FromBackendRow Sqlite Username where
  fromBackendRow = Username <$> fromBackendRow

instance HasSqlEqualityCheck Sqlite Username

instance BeamMigrateSqlBackend be =>
         HasDefaultSqlDataType be Username where
  defaultSqlDataType =
    defaultSqlDataType . fmap (T.pack . show)

usernameDataType :: DataType Sqlite Username
usernameDataType = DataType sqliteTextType

newtype Email =
  Email
    { _email :: Text
    } deriving (Show, Eq)

instance HasSqlValueSyntax be Text =>
         HasSqlValueSyntax be Email where
  sqlValueSyntax = sqlValueSyntax .
    (_email :: Email -> Text)

instance FromBackendRow Sqlite Email where
  fromBackendRow = Email <$> fromBackendRow

instance HasSqlEqualityCheck Sqlite Email

instance BeamMigrateSqlBackend be =>
         HasDefaultSqlDataType be Email where
  defaultSqlDataType = defaultSqlDataType .
    fmap (T.pack . show)

emailDataType :: DataType Sqlite Email
emailDataType = DataType sqliteTextType

data PersonT f =
  Person
    { _personUsername :: Columnar f Username
    , _personAge :: Columnar f Int
    , _personEmail :: C f Email
    } deriving (Generic, Beamable)

type Person = PersonT Identity
deriving instance Show Person
deriving instance Eq Person

instance Table PersonT where
  data PrimaryKey PersonT f =
    PersonId (Columnar f Username)
    deriving (Generic, Beamable)
  primaryKey = PersonId . _personUsername

newtype PersonDb f =
  PersonDb
    { _persons :: f (TableEntity PersonT)
    } deriving (Generic, Database be)

personDb :: DatabaseSettings Sqlite PersonDb
personDb = unCheckDatabase $
  evaluateDatabase initialSetupStep

initialSetup ::
  Migration Sqlite
    (CheckedDatabaseSettings Sqlite PersonDb)
initialSetup =
  PersonDb
    <$>
      (createTable "persons" $ Person
          { _personUsername =
            field "username" usernameDataType notNull
          , _personAge =
            field "age" int notNull
          , _personEmail =
            field "email" emailDataType notNull
          }
        )

initialSetupStep
  :: MigrationSteps Sqlite ()
    (CheckedDatabaseSettings Sqlite PersonDb)
initialSetupStep =
  migrationStep "initialSetup" $ const initialSetup

allowDestructive :: Monad m => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True }

migrateDb ::
  SqliteM (Maybe (CheckedDatabaseSettings Sqlite PersonDb))
migrateDb =
  bringUpToDateWithHooks allowDestructive
    migrationBackend initialSetupStep

maybeReadPerson
  :: forall (m :: * -> *)
   . MonadBeam Sqlite m
  => Username
  -> m (Maybe (PersonT Identity))
maybeReadPerson account =
  runSelectReturningOne $
    lookup_ (_persons personDb) (PersonId account)

createPerson username age email =
  runInsert $ insert (_persons personDb) $
    insertValues [Person username age email]

updatePersonEmail
  :: forall  (m :: * -> *). MonadBeam Sqlite m =>
    Username -> Email -> m ()
updatePersonEmail username email = do
  maybePerson <- maybeReadPerson username
  case maybePerson of
    Just person -> do
      let newPerson = person { _personEmail = email }
      runUpdate $ save (_persons personDb) newPerson
    Nothing -> pure ()

deletePerson
  :: forall  (m :: * -> *). MonadBeam Sqlite m =>
    Username -> m ()
deletePerson username =
  runDelete
    $ delete (_persons personDb) $
    \person ->
      _personUsername person ==. val_ username
