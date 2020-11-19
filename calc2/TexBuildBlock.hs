
module TexBuildBlock (buildBlock, buildBlockSkip)
       where


import DataInfo
import DataBlock
import TexCommon (Tex, bs, nl, pnl)


buildBlock :: Info -> Block a -> Tex -> Tex
buildBlock i (Block (hintAbove, hintBelow) _ _) tex
  = concat [buildHint i hintAbove,
            tex,
            buildHint i hintBelow]


buildHint :: Info -> Hint -> Tex
buildHint _ HiNone
  = []
buildHint _ (HiSpace points)
  = concat [bs, "vspace", "{", points, "pt", "}", pnl]
buildHint _ HiPageBreak
  = concat [bs, "pagebreak", pnl]


buildBlockSkip :: Info -> Block a -> Tex -> Tex
buildBlockSkip i (Block (hintAbove, hintBelow) _ _) tex
  = concat [buildHintSkip i hintAbove,
            tex,
            buildHintSkip i hintBelow]


buildHintSkip :: Info -> Hint -> Tex
buildHintSkip _ HiNone
  = []
buildHintSkip _ (HiSpace points)
  = concat [bs, "setlength", "{", bs, "skipdipson", "}",
            "{", points, "pt", "}",
            bs, "addtolength", "{", bs, "skipdipson", "}",
            "{", "-", bs, "baselineskipRecord", "}",
            bs, "hspace", "{", "0pt", "}",
            bs, bs, "[", bs, "skipdipson", "]", pnl]
buildHintSkip _ HiPageBreak
  = concat [nl, bs, "pagebreak", pnl, nl]


