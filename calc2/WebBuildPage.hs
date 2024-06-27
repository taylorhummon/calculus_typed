
module WebBuildPage (buildPage)
       where

import Compose
import DataInfo
import DataPage
import DataLocation

import WebCommon (Html, nl, htmlWrapper)
import WebBuildGroup (buildGroups)
import WebBuildToc (buildToc)
import WebBuildPhrase (buildPhrase)
import WebBuildNavigation (NavHeadFoot(..), buildNavigation)
import Info (fetchPageInfo, formatTitle)


buildPage :: Info -> Page -> Html
buildPage i (page@(Page loc _ subpages))
  = concat [buildNavigation i Header page,
            header i loc,
            buildToc i loc,
            concatMap (buildSubPage i) subpages,
            buildNavigation i Footer page]
    $> htmlWrapper i page


header :: Info -> Location -> String
header i loc
  =  concat ["<h1>",
             formatTitle (fetchPageInfo i loc),
            "</h1>", nl]


buildSubPage :: Info -> SubPage -> Html
buildSubPage i (SubPage (Just title) groups)
  = concat ["<h2>", buildPhrase title, "</h2>", nl,
            buildGroups i groups]
buildSubPage i (SubPage Nothing groups)
  = buildGroups i groups
