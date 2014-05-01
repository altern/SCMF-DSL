module DeploymentRule where

import VersionFilter
import Platform

data DeploymentRule = DeploymentRule VersionFilter Platform
                    deriving (Show)