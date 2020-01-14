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

newtype Age =
  Age
    { age :: Int
    }

data Person =
  Person
    { name  :: Name
    , age   :: Age
    , email :: Email
    }
