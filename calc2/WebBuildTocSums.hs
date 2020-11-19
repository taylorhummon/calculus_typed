
module WebBuildTocSums (buildTocSums)
       where

import DataInfo
import DataLocation

import Info (infoGetPageInfos)
import WebCommon (Html, nl)
import WebBuildTocCommon (buildEntry)


buildTocSums :: Info -> Location -> Html
buildTocSums info _
  = concat ["<ul>", nl,
            concatMap (summaryToHtml info) (getSummaries info),
            "</ul>", nl]


summaryToHtml :: Info -> PageInfo -> Html
summaryToHtml info pageInfo
  = concat ["<li>", nl,
            buildEntry info pageInfo, nl,
            "</li>", nl]


getSummaries :: Info -> [PageInfo]
getSummaries i
  = filter isSummary (infoGetPageInfos i)


isSummary :: PageInfo -> Bool
isSummary (PageInfo _ _ PtSummary)
  = True
isSummary _
  = False


