module MaturityLevel where

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Prod
                   deriving (Show, Enum, Ord, Eq)

incrementMaturityLevel :: MaturityLevel -> MaturityLevel
incrementMaturityLevel Prod = Prod
incrementMaturityLevel ml = succ ml

decrementMaturityLevel :: MaturityLevel -> MaturityLevel
decrementMaturityLevel Dev = Dev
decrementMaturityLevel ml = pred ml