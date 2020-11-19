
module WebBuildTocApps (buildTocApps)
       where

import DataInfo
import DataLocation

import Info (infoGetPageInfos)
import WebCommon (Html, nl)
import WebBuildTocCommon (buildEntry)


buildTocApps :: Info -> Location -> Html
buildTocApps info _
  = concat ["<ul>", nl,
            concatMap (appendixToHtml info) (getAppendixes info),
            "</ul>", nl]


appendixToHtml :: Info -> PageInfo -> Html
appendixToHtml info pageInfo
  = concat ["<li>", nl,
            buildEntry info pageInfo, nl,
            "</li>", nl]


getAppendixes :: Info -> [PageInfo]
getAppendixes i
  = filter isAppendix (infoGetPageInfos i)


isAppendix :: PageInfo -> Bool
isAppendix (PageInfo _ _ (PtAppendix _))
  = True
isAppendix _
  = False


