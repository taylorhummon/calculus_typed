
module WebLocations (getLocsToProcess)
       where

import Compose
import DataInfo
import DataLocation

import Locations (allLocsExceptParts)
import WebFiles (isPageOutDated)

import Control.Monad (filterM)


getLocsToProcess :: Info -> IO [Location]
getLocsToProcess info
  = info
    $> allLocsExceptParts
    $> filterM (isPageOutDated info)


