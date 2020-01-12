{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Person where

import           Control.Arrow              (returnA)
import           Control.Monad              (void)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PGS
import           Data.Int                    (Int64)
import           Opaleye

type TableWriteRead a = Table a a

newtype User' a =
  User
    { user :: a
    }

type User = User' Text

$(makeAdaptorAndInstance "pUser" ''User')

type UserField = User' (Field SqlText)

newtype Email' a =
  Email
    { email :: a
    }

type Email = Email' Text

$(makeAdaptorAndInstance "pEmail" ''Email')

type EmailField = Email' (Field SqlText)

newtype Age' a =
  Age
    { age :: a
    }

type Age = Age' Int

$(makeAdaptorAndInstance "pAge" ''Age')

type AgeField = Age' (Field SqlInt4)

data Person' a b c =
  Person
    { _user  :: a
    , _email :: b
    , _age   :: c
    }

type Person = Person' User Email Age

$(makeAdaptorAndInstance "pPerson" ''Person')

type PersonField = Person' UserField EmailField AgeField

personTable :: TableWriteRead PersonField
personTable =
  table
    "persons"
    (pPerson
       Person
         { _user = pUser (User (tableField "name"))
         , _email = pEmail (Email (tableField "email"))
         , _age = pAge (Age (tableField "age"))
         })

-- Fixture
createPersonTable :: PGS.Connection -> IO Int64
createPersonTable conn =
  PGS.execute_
    conn
      "CREATE TABLE \"persons\"\
      \( name TEXT PRIMARY KEY NOT NULL\
      \, email TEXT NOT NULL\
      \, age INT NOT NULL\
      \)"

deleteUser :: User -> Delete Int64
deleteUser (User name) =
  Delete
    { dTable = personTable
    , dWhere =
        \Person {_user = User {user = name'}}
          -> name' .=== sqlStrictText name
    , dReturning = rCount
    }

insertPerson :: User -> Email -> Age -> Insert Int64
insertPerson (User u) (Email e) (Age a) =
  Insert
    { iTable = personTable
    , iRows =
        [ Person
            { _user = User . sqlStrictText $ u
            , _email = Email . sqlStrictText $ e
            , _age = Age . sqlInt4 $ a
            }
        ]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

updateUserEmailAge :: User -> Email -> Age -> Update Int64
updateUserEmailAge (User name) (Email email) (Age age) =
  Update
    { uTable = personTable
    , uUpdateWith = \person ->
        person { _email = Email . sqlStrictText $ email 
               , _age = Age . sqlInt4 $ age
               }
    , uWhere =
        \Person {_user = User {user = name'}} ->
          name' .=== sqlStrictText name
    , uReturning = rCount
    }

selectUser :: User -> Select PersonField
selectUser (User name) =
  proc () -> do
    person@Person {_user = User{user = name'}} <-
      selectTable personTable -< ()
    restrict -< name' .=== sqlStrictText name
    returnA -< person
