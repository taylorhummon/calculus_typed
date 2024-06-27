
module WebBuildTocCommon (buildEntry, subLocation)
       where

import DataInfo
import DataLink
import DataLocation

import WebCommon (Html)
import WebBuildLink (buildLink)
import Info (formatTitle)


buildEntry :: Info -> PageInfo -> Html
buildEntry info (pageInfo@(PageInfo loc _ _))
  = buildLink info (LkString loc title)
  where title = formatTitle pageInfo


subLocation :: Location -> Location -> Bool
subLocation [] _
  = True
subLocation _ []
  = False
subLocation (pageId : rest) (pageId' : rest')
  = pageId == pageId' && subLocation rest rest'
