{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Person
import           Database.PostgreSQL.Simple
import           Control.Monad                  ( void )
import Opaleye (runInsert_, runDelete_, runUpdate_, runSelect)

dropTable :: Connection -> IO ()
dropTable conn = 
  void $ execute_ conn "DROP TABLE IF EXISTS person"

createTestFixtures :: Connection -> IO ()
createTestFixtures conn = do
  dropTable conn
  void $ createPersonTable conn

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectUser     = "bmooreii"
                                     , connectDatabase = "test"
                                     , connectPassword = "bmooreii"
                                     }
  createTestFixtures conn
  hspec $ describe "Basic CRUD Operations" $ do
    it "should create a person" $ do
      result <- runInsert_ conn
                  (insertPerson
                    (User "bmooreii")
                    (Email "dummy@dummy.com")
                    (Age 31)
                  )
      result `shouldBe` 1
    it "should read a person" $ do
      result <- runSelect conn
        (selectUser $ User "bmooreii") :: IO [Person]
      length result `shouldBe` 1
    it "should update a person" $ do
      result <- runUpdate_ conn
        (updateUserEmailAge
          (User "bmooreii")
          (Email "newemail@newemail.com")
          (Age 30)
        )
      result `shouldBe` 1
    it "should delete a person" $ do
      result <- runDelete_ conn
        (deleteUser
          (User "bmooreii")
        )
      result `shouldBe` 1
  dropTable conn
  close conn
