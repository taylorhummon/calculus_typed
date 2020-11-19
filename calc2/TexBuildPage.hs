
module TexBuildPage (buildPage)
       where

import DataInfo
import DataPage
import DataLocation

import TexCommon (Tex, bs, nl, pnl)
import TexBuildGroup (buildGroups)
import TexBuildPhrase (buildPhrase)
import TexBuildLink (buildAnchor)
import Info (fetchPageInfo)


buildPage :: Info -> Page -> Tex
buildPage i (Page loc _ subpages)
  = concat [buildHeader i loc,
            buildLabel loc,
            buildSubPages i subpages]


buildLabel :: Location -> Tex
buildLabel loc
  =  concat [bs, "label", "{", buildAnchor loc, "}", pnl]


buildHeader :: Info -> Location -> Tex
buildHeader i loc
  = case pt
    of PtTop
         -> concat [bs, "chapter*{Introduction}", pnl]
       PtPart _
         -> concat [bs, "part{", title, "}", pnl]
       PtChapter _
         -> concat [bs, "chapter{", title, "}", pnl]
       PtSection _ _
         -> concat [bs, "section{", title, "}", pnl]
       PtSummaries
         -> concat [bs, "part*{Summaries}", pnl,
                    bs, "addcontentsline{toc}{part}{Summaries}", pnl]
       PtSummary
         -> concat [bs, "chapter{", title, "}", pnl]
       PtAppendices
         -> concat [bs, "part*{Appendices}", pnl,
                    bs, "addcontentsline{toc}{part}{Appendices}", pnl]
       PtAppendix _
         -> concat [bs, "chapter{", title, "}", pnl]
       PtUnknown
         -> ""
  where (PageInfo _ title pt) = fetchPageInfo i loc


buildSubPages :: Info -> [SubPage] -> Tex
buildSubPages i subpages
  = concatMap (buildSubPage i) subpages


buildSubPage :: Info -> SubPage -> Tex
buildSubPage i (SubPage (Just title) groups)
  = concat [bs, "subsection*{", buildPhrase title, "}", pnl,
            buildGroups i groups, nl]
buildSubPage i (SubPage Nothing groups)
  = concat [buildGroups i groups, nl]


