
module WebBuildGroup (buildGroups)
       where

import DataInfo
import DataGroup
import DataLayout
import DataBlock

import GroupTypes (fancyGroupTypes)
import WebCommon (Html, quote, nl)
import WebBuildLayout (buildLayouts)
import WebBuildBlock (buildDiv)


buildGroups :: Info -> Groups -> Html
buildGroups i
  = concatMap (buildGroup i)


buildGroup :: Info -> Block Group -> Html
buildGroup i (b @ (Block _ _ (Group GtPlain layouts)))
  = buildDiv i "group plain" b (buildLayouts i layouts)
buildGroup i (b @ (Block _ _ (Group gt layouts)))
  = case lookup gt fancyGroupTypes
    of Just (name, pretty)
         -> buildDiv i ("group" ++ " " ++ name) b (buildInnards i pretty layouts)
       Nothing
         -> error "Unknown group type."


buildInnards :: Info -> String -> Layouts -> Html
buildInnards i pretty layouts
  = concat [buildTitle pretty,
            buildLayouts i layouts]


buildTitle :: String -> Html
buildTitle pretty
  = concat ["<div class=", quote, "head", quote, ">",
            pretty,
            "</div>", nl]

