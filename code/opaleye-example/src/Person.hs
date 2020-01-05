{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Person where

import Opaleye
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PGS
import GHC.Int (Int64)
import           Control.Arrow                  ( returnA )
import           Control.Monad                  ( void )

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
    { _user :: a
    , _email :: b
    , _age :: c
    }
type Person = Person' User Email Age
$(makeAdaptorAndInstance "pPerson" ''Person')
type PersonField = Person' UserField EmailField AgeField

-- FIXME: We need to make sure our `_user` field is tagged as UNIQUE

personTable :: TableWriteRead PersonField
personTable = table "personTable"
  (pPerson
    Person
      { _user = pUser (User (tableField "user"))
      , _email = pEmail (Email (tableField "email"))
      , _age = pAge (Age (tableField "age"))
      }
  )

deleteUser :: User -> Delete Int64
deleteUser (User name) =
  Delete 
    { dTable = personTable
    , dWhere = \Person { _user = User {user = name'} } -> name' .=== sqlStrictText name
    , dReturning = rCount
    }

insertUser :: User -> Email -> Age -> Insert Int64
insertUser (User u) (Email e) (Age a) = Insert
  { iTable = personTable
  , iRows = [ Person
                { _user = User . sqlStrictText $ u
                , _email = Email . sqlStrictText $ e
                , _age = Age . sqlInt4 $ a }
            ]
  , iReturning = rCount
  , iOnConflict = Nothing
  }

updateUser :: User -> Update Int64
updateUser (User name) =
  Update 
    { uTable = personTable
    , uUpdateWith = \person -> person { _user = User . sqlStrictText $ name }
    , uWhere = \Person { _user = User {user = name'} } -> name' .=== sqlStrictText name
    , uReturning = rCount
    }

selectUser :: User -> Select PersonField
selectUser (User name) = proc () -> do
  person@Person { _user = User { user = name'} } <- selectTable personTable -< ()
  restrict -< name' .=== sqlStrictText name
  returnA -< person

-- FIXME: update run functions to return modified UNIQUE _user

runDeletePerson
  :: PGS.Connection
  -> Delete Int64
  -> IO ()
runDeletePerson = (void .) . runDelete_

runInsertPerson
  :: PGS.Connection
  -> Insert Int64
  -> IO ()
runInsertPerson = (void .) . runInsert_

runUpdatePerson
  :: PGS.Connection
  -> Update Int64
  -> IO ()
runUpdatePerson = (void .) . runUpdate_

runSelectPerson
  :: PGS.Connection
  -> Select PersonField
  -> IO [Person]
runSelectPerson = runSelect
