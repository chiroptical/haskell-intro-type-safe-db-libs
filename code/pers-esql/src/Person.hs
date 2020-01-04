{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Person where

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (ReaderT)
import           Data.Text               (Text)
import           Database.Esqueleto
import qualified Database.Persist        as P
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH     as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
Username
  username Text

Email
  email Text

Age
  age Int

Person
  username Username
  age Age
  email Email
  UniqueUsername username
|]

deriving instance Show Username
deriving instance Eq Username
deriving instance Show Email
deriving instance Eq Email
deriving instance Show Age
deriving instance Eq Age
deriving instance Show Person
deriving instance Eq Person

mkTable :: MonadIO m => ReaderT SqlBackend m ()
mkTable = runMigration migrateAll

createUsername
  :: MonadIO m
  => Username
  -> Age
  -> Email
  -> ReaderT SqlBackend m (Key Person)
createUsername name age email = insert (Person name age email)

readUsername
  :: MonadIO m
  => Username
  -> Sqlite.SqlPersistT m [Entity Person]
readUsername name =
  select $
  from $ \p -> do
    where_ (p ^. PersonUsername ==. val name)
    return p

updateUsername
  :: MonadIO m
  => Username
  -> Username
  -> Sqlite.SqlPersistT m ()
updateUsername prevUsername updatedUsername =
  update $ \p -> do
    set p [PersonUsername =. val updatedUsername]
    where_ (p ^. PersonUsername ==. val prevUsername)

deleteUsername
  :: MonadIO m
  => Username
  -> Sqlite.SqlPersistT m ()
deleteUsername name =
  delete $
    from $ \p ->
      where_ (p ^. PersonUsername ==. val name)
