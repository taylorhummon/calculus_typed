
module WebBuildNavigation (NavHeadFoot(..), buildNavigation)
       where

import DataInfo
import DataPage

import WebCommon (Html, quote, nl)
import WebBuildLink (buildPrevLink, buildUpLink, buildNextLink)

data NavHeadFoot = Header | Footer


buildNavigation :: Info -> NavHeadFoot -> Page -> Html
buildNavigation i hf page
  = concat ["<div class=", quote, classname hf, quote, ">", nl,
            buildPrevLink i page, ".", nl,
            buildUpLink i page, ".", nl,
            buildNextLink i page, ".", nl,
            "</div>", nl]
  where classname :: NavHeadFoot -> String
        classname Header = "navigation header"
        classname Footer = "navigation footer"

