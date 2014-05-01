module VersionNumber where

data VersionNumber = NumberPlaceholder                      -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)
 
versionNumberToString :: VersionNumber -> String
versionNumberToString (Number n) = (show n)
versionNumberToString NumberPlaceholder = "X"

instance Eq VersionNumber where
    NumberPlaceholder == NumberPlaceholder = True
    (Number v1) == (Number v2)            = (v1 == v2)
    _ == _                                 = False
    
instance Ord VersionNumber where
    NumberPlaceholder `compare` NumberPlaceholder = error "Cannot compare number placeholders"
    (Number _ ) `compare` NumberPlaceholder = error "Cannot compare numbers and number placeholders"
    NumberPlaceholder `compare` (Number _ ) = error "Cannot compare number placeholders and numbers"
    (Number v1) `compare` (Number v2) = (v1 `compare` v2)