
module WebBuildTocChap (buildTocChap)
       where

import DataInfo
import DataLocation

import Info (infoGetPageInfos)
import WebCommon (Html, nl)
import WebBuildTocCommon (buildEntry, subLocation)


buildTocChap :: Info -> Location -> Html
buildTocChap info loc
  = concat ["<ul>", nl,
            concatMap (sectionToHtml info) (getSections info loc),
            "</ul>", nl]


getSections :: Info -> Location -> [PageInfo]
getSections i loc
  = filter (isSectionIn loc) (infoGetPageInfos i)

isSectionIn :: Location -> PageInfo -> Bool
isSectionIn loc (PageInfo loc' _ (PtSection _ _))
  = loc `subLocation` loc'
isSectionIn _ _
  = False


sectionToHtml :: Info -> PageInfo -> Html
sectionToHtml info pageInfo
  = concat ["<li>", nl,
            buildEntry info pageInfo, nl,
            "</li>", nl]


