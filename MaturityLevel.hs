module MaturityLevel where

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Prod
                   deriving (Show, Enum, Ord, Eq)
                   