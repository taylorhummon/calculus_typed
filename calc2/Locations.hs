
module Locations (allLocs
                 ,allLocsExceptParts
                 ,frontLocs
                 ,mainLocs
                 ,appendLocs
                 ,backLocs)
       where

import Compose
import DataInfo
import DataLocation

import Info (infoGetPageInfos)


getLoc :: PageInfo -> Location
getLoc (PageInfo loc _ _)
  = loc


allLocs :: Info -> [Location]
allLocs
  = infoGetPageInfos
    .> map getLoc 


allLocsExceptParts :: Info -> [Location]
allLocsExceptParts
  = infoGetPageInfos
    .> filter (isPart .> not)
    .> map getLoc


frontLocs :: Info -> [Location]
frontLocs
  = infoGetPageInfos
    .> filter isFront
    .> map getLoc


mainLocs :: Info -> [Location]
mainLocs
  = infoGetPageInfos
    .> filter isMain
    .> map getLoc


appendLocs :: Info -> [Location]
appendLocs
  = infoGetPageInfos
    .> filter isAppend
    .> map getLoc


backLocs :: Info -> [Location]
backLocs
  = infoGetPageInfos
    .> filter isBack
    .> map getLoc


isPart :: PageInfo -> Bool
isPart (PageInfo _ _ (PtPart _))
  = True
isPart _
  = False


isFront :: PageInfo -> Bool
isFront (PageInfo _ _ PtTop)
  = True
isFront _
  = False


isMain :: PageInfo -> Bool
isMain (PageInfo _ _ (PtPart _))
  = True
isMain (PageInfo _ _ (PtChapter _))
  = True
isMain (PageInfo _ _ (PtSection _ _))
  = True
isMain _
  = False


isAppend :: PageInfo -> Bool
isAppend (PageInfo _ _ PtAppendices)
  = True
isAppend (PageInfo _ _ (PtAppendix _))
  = True
isAppend _
  = False


isBack :: PageInfo -> Bool
isBack (PageInfo _ _ PtSummaries)
  = True
isBack (PageInfo _ _ PtSummary)
  = True
isBack _
  = False

