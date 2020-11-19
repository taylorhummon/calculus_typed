
module TexBuildPassage (buildPassages, buildTable)
       where

import Compose
import DataInfo
import DataPassage
import DataBlock

import Data.List (intersperse)
import TexCommon (Tex, bs, nl, pnl)
import TexBuildMatter (buildMatters)
import TexBuildWriting (buildText)
import TexBuildBlock (buildBlock)


buildPassages :: Info -> Passages -> Tex
buildPassages i passages
  = passages
    $> map (buildBlockPassage i)
    $> intersperse nl
    $> ([nl] ++)
    $> (++ [nl])
    $> concat


buildBlockPassage :: Info -> Block Passage -> Tex
buildBlockPassage i (b @ (Block _ _ passage))
  = buildBlock i b (buildPassage i passage)


buildPassage :: Info -> Passage -> Tex
buildPassage i (PaParagraph matters)
  = buildMatters i matters
buildPassage i (PaTable table)
  = concat [bs, "vspace", "{", "5pt", "}", pnl,
            bs, "begin{center}", pnl,
            buildTable i table,
            bs, "end{center}", pnl,
            bs, "vspace", "{", "5pt", "}", pnl]


buildTable :: Info -> Table -> Tex
buildTable i (Table TableShort format header rows)
  = concat [bs, "begin{tabu}", "{", rowFormat format, "}", pnl,
            bs, "hline", pnl,
            buildHeaderShort i header,
            bs, "hline", pnl,
            buildRowsShort i rows,
            bs, "hline", pnl,
            bs, "end{tabu}", pnl]
buildTable i (Table TableLong format header rows)
  = concat [bs, "begin{longtabu}", "{", rowFormat format, "}", pnl,
            bs, "hline", pnl,
            buildHeaderLong i header,
            bs, "hline", pnl,
            buildRowsLong i rows,
            bs, "hline", pnl,
            bs, "end{longtabu}", pnl]


rowFormat :: [Alignment] -> String
rowFormat
  = map alignmentToChar
    .> intersperse '|'
    .> (++ "|")
    .> ("|" ++)


alignmentToChar :: Alignment -> Char
alignmentToChar AlignLeft
  = 'l'
alignmentToChar AlignCenter
  = 'c'
alignmentToChar AlignRight
  = 'r'


buildHeaderShort :: Info -> TableRow -> Tex
buildHeaderShort
  = buildRowShort


buildRowsShort :: Info -> [TableRow] -> Tex
buildRowsShort i
  = concatMap (buildRowShort i)


buildRowShort :: Info -> TableRow -> Tex
buildRowShort i row
  = concat [buildCells i row, nl,
            bs, bs, nl]


buildHeaderLong :: Info -> TableRow -> Tex
buildHeaderLong i row
  = concat [buildCells i row, nl,
            bs, bs, nl]


buildRowsLong :: Info -> [TableRow] -> Tex
buildRowsLong i
  = concatMap (buildRowLong i)


buildRowLong :: Info -> TableRow -> Tex
buildRowLong i row
  = concat [bs, "strutB", bs, "strutT", pnl,
            buildCells i row, nl,
            bs, bs, nl]


buildCells :: Info -> [TableCell] -> Tex
buildCells _ []
  = []
buildCells i (cell : [])
  = buildText i cell
buildCells i (cell : rest)
  = concat [buildText i cell,
            nl, "&", nl,
            buildCells i rest]

