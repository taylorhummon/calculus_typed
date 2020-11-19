
module WebBuildToc (buildToc)
       where

import DataInfo
import DataLocation

import Info (fetchPageInfo)
import WebCommon (Html, nl, quote)
import WebBuildTocTop (buildTocTop)
import WebBuildTocChap (buildTocChap)
import WebBuildTocSums (buildTocSums)
import WebBuildTocApps (buildTocApps)


buildToc :: Info -> Location -> Html
buildToc info loc
  = case fetchPageInfo info loc
    of PageInfo _ _ PtTop
         -> surroundWithDiv (buildTocTop info)
       PageInfo _ _ (PtChapter _)
         -> surroundWithDiv (buildTocChap info loc)
       PageInfo _ _ PtSummaries
         -> surroundWithDiv (buildTocSums info loc)
       PageInfo _ _ PtAppendices
         -> surroundWithDiv (buildTocApps info loc)
       _
         -> ""


surroundWithDiv :: Html -> Html
surroundWithDiv html
  = concat ["<div class=", quote, "toc", quote, ">", nl,
            html,
            "</div>", nl]

