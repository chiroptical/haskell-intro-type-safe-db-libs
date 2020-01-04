{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Person

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Identity   ( IdentityT(..) )
import qualified Database.Persist.Sqlite       as Sqlite
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                )
import           Test.Hspec
import qualified Data.Text                     as T

-- Fixtures
createFreshDb :: IO Bool
createFreshDb = do
  remove <- doesFileExist database'
  if remove then removeFile database' else pure ()
  liftIO $ Sqlite.runSqlite database mkTable
  doesFileExist database'

database' = "test.db"
database = T.pack database'
username = Username "barrymoo"
age = Age 31
email = Email "moore0557@gmail.com"
person = Person username age email
newUsername = username { usernameUsername = "chiroptical" }
newPerson = person { personUsername = newUsername }

main :: IO ()
main = hspec $ describe "Basic CRUD Operations" $ do
  it "fixture: create empty database" $ do
    result <- createFreshDb
    result `shouldBe` True
  it "should create a Person" $ do
    liftIO $ Sqlite.runSqlite database $
      createUsername username age email
    entities <- liftIO $ Sqlite.runSqlite database $
      readUsername username
    length entities `shouldBe` 1
    (Sqlite.entityVal <$> entities) `shouldBe` [person]
  it "should update the Username" $ do
    liftIO $ Sqlite.runSqlite database $
      updateUsername username newUsername
    entities <- liftIO $ Sqlite.runSqlite database $
      readUsername newUsername
    length entities `shouldBe` 1
    (Sqlite.entityVal <$> entities) `shouldBe` [newPerson]
  it "should delete a Person" $ do
    liftIO $ Sqlite.runSqlite database $
      deleteUsername newUsername
    entities <- liftIO $ Sqlite.runSqlite database $
      readUsername newUsername
    length entities `shouldBe` 0
