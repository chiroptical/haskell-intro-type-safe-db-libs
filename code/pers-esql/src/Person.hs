{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
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
  deriving Show

Email
  email Text
  deriving Show

Age
  age Int
  deriving Show

Person
  username Username
  age Age
  email Email
  UniqueUsername username
  deriving Show
|]

mkTable :: IO ()
mkTable = Sqlite.runSqlite "person.db" $ runMigration migrateAll

create ::
     MonadIO m => Username -> Age -> Email -> ReaderT SqlBackend m (Key Person)
create name age email = insert (Person name age email)

read :: MonadIO m => Username -> Sqlite.SqlPersistT m [Entity Person]
read name =
  select $
  from $ \p -> do
    where_ (p ^. PersonUsername ==. val name)
    return p

update :: MonadIO m => Username -> Sqlite.SqlPersistT m ()
update = undefined

delete :: MonadIO m => Username -> Sqlite.SqlPersistT m ()
delete = undefined
