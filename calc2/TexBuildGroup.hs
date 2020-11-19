
module TexBuildGroup (buildGroups)
       where

import Compose
import DataInfo
import DataGroup
import DataBlock

import Data.List (intersperse)
import GroupTypes (fancyGroupTypes)
import TexCommon (Tex, bs, nl, pnl)
import TexBuildLayout (buildLayouts)
import TexBuildBlock (buildBlock)


buildGroups :: Info -> Groups -> Tex
buildGroups i groups
  = groups
    $> map (buildBlockGroup i)
    $> intersperse nl
    $> ([nl] ++)
    $> (++ [nl])
    $> concat


buildBlockGroup :: Info -> Block Group -> Tex
buildBlockGroup i (b @ (Block _ _ group))
   = buildBlock i b (buildGroup i group)


buildGroup :: Info -> Group -> Tex
buildGroup i (Group GtPlain layouts)
  = concat [bs, "begin{addmargin}", "[", indent, "]", "{", indent, "}", pnl,
            buildLayouts i layouts,
            bs, "end{addmargin}", pnl]
    where indent :: String
          indent = bs ++ "plainGroupIndent"

buildGroup i (Group gt layouts)
  = case lookup gt fancyGroupTypes
    of Just (str, _)
         -> concat [bs, "begin{longfbox}", "[", str, "]", pnl,
                    bs, "begin{", str, "}", pnl,
                    bs, "hspace{0pt}",
                    bs, bs, "[", "-", bs, "baselineskipRecord", "]",
                    bs, "vspace", "{", "-4pt", "}", pnl,
                    buildLayouts i layouts,
                    bs, "end{", str, "}", pnl,
                    bs, "end{longfbox}", pnl]
       Nothing
         -> error "Unknown group type."


