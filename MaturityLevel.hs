module MaturityLevel where

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Stable
                   deriving (Show, Enum, Ord, Eq)
                   