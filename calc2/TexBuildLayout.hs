
module TexBuildLayout (buildLayouts)
       where

import Compose
import DataInfo
import DataLayout
import DataBlock

import Data.List (intersperse)
import TexCommon (Tex, bs, nl, pnl)
import TexBuildPassage (buildPassages, buildTable)
import TexBuildMatter (buildImage)
import TexBuildBlock (buildBlock)
import TexBuildWriting (buildText)
import Block (unpackBlock)


buildLayouts :: Info -> Layouts -> Tex
buildLayouts i layouts
  = layouts
    $> map (buildBlockLayout i)
    $> intersperse nl
    $> ([nl] ++)
    $> (++ [nl])
    $> concat


buildBlockLayout :: Info -> Block Layout -> Tex
buildBlockLayout i (b@(Block _ _ layout))
  = buildBlock i b (buildLayout i layout)


buildLayout :: Info -> Layout -> Tex
buildLayout i (LtPlain passages)
  = buildPassages i passages
buildLayout i (LtTB entrys)
  = concat [bs, "begin{description}", pnl,
            tex,
            bs, "end{description}", pnl]
    where tex = entrys
                $> map (buildBlockEntryTB i)
                $> intersperse nl
                $> ([nl] ++)
                $> (++ [nl])
                $> concat
buildLayout i (LtBF entrys)
  = concat [tex,
            bs, "vspace{5pt}", pnl]
    where tex = entrys
                $> map (buildBlockEntryBF i)
                $> intersperse (concat [nl, bs, "vspace{3pt}", pnl, nl])
                $> ([nl] ++)
                $> (++ [nl])
                $> concat
buildLayout i (LtTBF entrys)
  = concat [bs, "begin{description}", pnl,
            tex,
            bs, "end{description}", pnl,
            bs, "vspace{5pt}", pnl]
    where tex = entrys
                $> map (buildBlockEntryTBF i)
                $> intersperse (concat [nl, bs, "vspace{3pt}", pnl, nl])
                $> ([nl] ++)
                $> (++ [nl])
                $> concat


buildBlockEntryTB :: Info -> Block EntryTB -> Tex
buildBlockEntryTB i (b@(Block _ _ entry))
  = buildBlock i b (buildEntryTB i entry)


buildEntryTB :: Info -> EntryTB -> Tex
buildEntryTB i (EntryTB bt bb)
  = concat [bs, "item", "[{",
            bs, "uline", "{",
            buildBlockTitle i bt,
            "}",
            "}]", pnl,
            bs, "hspace{0pt}",
            bs, bs, "[", "-", bs, "baselineskipRecord]",
            bs, "vspace{-3pt}", pnl,
            buildBlockBody i bb]


buildBlockEntryBF :: Info -> Block EntryBF -> Tex
buildBlockEntryBF i (b@(Block _ _ entry))
  = buildBlock i b (buildEntryBF i entry)


buildEntryBF :: Info -> EntryBF -> Tex
buildEntryBF i (EntryBF bb bf)
  = concat [minipage SideLeft "BF" (buildBlockBody i bb),
            bs, "hspace{5pt}", pnl,
            minipage SideRight "BF" (buildBlockFigure i bf)]


buildBlockEntryTBF :: Info -> Block EntryTBF -> Tex
buildBlockEntryTBF i (b@(Block _ _ entry))
  = buildBlock i b (buildEntryTBF i entry)


buildEntryTBF :: Info -> EntryTBF -> Tex
buildEntryTBF i (EntryTBF bt bb bf)
  = concat [bs, "item", "[{",
            bs, "uline", "{",
            buildBlockTitle i bt,
            "}",
            "}]", pnl,
            bs, "hspace{0pt}",
            bs, bs, "[", "-", bs, "baselineskipRecord]",
            bs, "vspace{-3pt}", pnl, nl,
            minipage SideLeft "TBF" (buildBlockBody i bb),
            bs, "hspace{5pt}", pnl,
            minipage SideRight "TBF" (buildBlockFigure i bf)]


buildBlockTitle :: Info -> Block Title -> Tex
buildBlockTitle i bt
  = bt
    $> unpackBlock
    $> buildTitle i


buildTitle :: Info -> Title -> Tex
buildTitle i title
  = buildText i title


buildBlockBody :: Info -> Block Body -> Tex
buildBlockBody i bb
  = bb
    $> unpackBlock
    $> buildBody i


buildBody :: Info -> Body -> Tex
buildBody i body
  = buildPassages i body


buildBlockFigure :: Info -> Block Figure -> Tex
buildBlockFigure i bf
  = bf
    $> unpackBlock
    $> buildFigure i


buildFigure :: Info -> Figure -> Tex
buildFigure i (FiImage bi)
  = bi
    $> unpackBlock
    $> buildImage i
    $> lowerByHeight
    $> raiseByTen
buildFigure i (FiTable bt)
  = bt
    $> unpackBlock
    $> buildTable i
    $> lowerByHeight
    $> raiseByTen


raiseByTen :: Tex -> Tex
raiseByTen tex
  = concat [bs, "raisebox", "{", "10pt", "}", "{", pnl,
            tex,
            "}", pnl]


lowerByHeight :: Tex -> Tex
lowerByHeight tex
  = concat [bs, "raisebox", "{", "-", bs, "height", "}","{", pnl,
            tex,
            "}", pnl]


data Side = SideLeft | SideRight


minipage :: Side -> String -> Tex -> Tex
minipage SideLeft str tex
  = concat [bs, "begin{minipage}[t]{", bs, "left", str, "}", pnl,
            bs, "setlength{", bs, "parskip", "}{", bs, "parskipRecord", "}", pnl,
            tex,
            bs, "end{minipage}", pnl]
minipage SideRight str tex
  = concat [bs, "begin{minipage}[t]{", bs, "right", str, "}", pnl,
            bs, "centering", pnl,
            tex,
            bs, "end{minipage}", pnl]
