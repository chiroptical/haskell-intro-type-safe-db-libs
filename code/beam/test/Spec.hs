{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Person
import Database.SQLite.Simple (open, close)
import Database.Beam.Sqlite.Connection (runBeamSqliteDebug)
import Filesystem (isFile, removeFile)

username :: Username
username = Username "chiroptical"

age :: Int
age = 31

email :: Email
email = Email "dummy@dummy.com"

newEmail :: Email
newEmail = Email "new@new.com"

person :: Person
person = Person username age email

newPerson :: Person
newPerson = Person username age newEmail

main :: IO ()
main = do
  exists <- isFile "test.db"
  if exists
     then removeFile "test.db"
     else pure ()
  conn <- open "test.db"
  runBeamSqliteDebug putStrLn conn migrateDb
  hspec $
    describe "Basic CRUD Tests" $ do
      it "inserts successfully" $ do
        runBeamSqliteDebug putStrLn conn (createPerson username age email)
        result <- runBeamSqliteDebug putStrLn conn (maybeReadPerson username)
        result `shouldNotBe` Nothing
      it "updates successfully" $ do
        runBeamSqliteDebug putStrLn conn (updatePersonEmail username newEmail)
        result <- runBeamSqliteDebug putStrLn conn (maybeReadPerson username)
        result `shouldNotBe` Nothing
      it "deletes successfully" $ do
        runBeamSqliteDebug putStrLn conn (deletePerson username)
        result <- runBeamSqliteDebug putStrLn conn (maybeReadPerson username)
        result `shouldBe` Nothing
  removeFile "test.db"
  close conn
