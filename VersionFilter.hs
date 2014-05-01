module VersionFilter where 

import Version
import MaturityLevel

data VersionNumberFilterCompound = FilterNumberPlaceholder
                           | FilterNumber Int
                           | EitherPlaceholderOrNumber
                           | AllNumbers
                           deriving (Show)

data VersionNumberFilter = VersionNumberFilterCompound VersionNumberFilterCompound
                   | SubFilter VersionNumberFilter VersionNumberFilterCompound
                   deriving (Show)
                   
data VersionFilter = SingleVersionFilter Version
                   | MaturityLevelFilter MaturityLevel
                   | MaturityLevelVersionFilter MaturityLevel Version
                   | PatternVersionFilter VersionNumberFilter
                   | MaturityLevelPatternFilter MaturityLevel VersionNumberFilter
        --         | RangeVersionFilter VersionRange
        --         | MaturityLevelRangeVersionFilter MaturityLevel VersionRange
                   deriving (Show)