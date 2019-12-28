{-# LANGUAGE DuplicateRecordFields #-}

module Person where

import           Data.Text (Text)

newtype Name =
  Name
    { name :: Text
    }

newtype Email =
  Email
    { email :: Text
    }

data Person =
  Person
    { name  :: Name
    , age   :: Int
    , email :: Email
    }
