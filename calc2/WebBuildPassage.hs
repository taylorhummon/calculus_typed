
module WebBuildPassage (buildPassages, buildTable)
       where

import DataInfo
import DataPassage
import DataBlock

import WebCommon (Html, quote, nl)
import WebBuildBlock (buildDiv)
import WebBuildMatter (buildMatters)
import WebBuildWriting (buildText)


buildPassages :: Info -> Passages -> Html
buildPassages i
  = concatMap (buildPassage i)


buildPassage :: Info -> Block Passage -> Html
buildPassage i (b@(Block _ _ (PaParagraph matters)))
  = buildDiv i "passage paragraph" b (buildMatters i matters)
buildPassage i (b@(Block _ _ (PaTable t)))
  = case t
    of (Table TableShort _ _ _)
         -> buildDiv i "passage table short" b (buildTable i t)
       (Table TableLong _ _ _)
         -> buildDiv i "passage table long" b (buildTable i t)


buildTable :: Info -> Table -> Html
buildTable i (Table _ format header rows)
  = concat ["<table>", nl,
            buildHeaderRow i format header,
            concatMap (buildTableRow i format) rows,
            "</table>", nl]


buildHeaderRow :: Info -> [Alignment] -> TableRow -> Html
buildHeaderRow i format row
  = concat ["<tr>", nl,
            concatMap (buildHeaderCell i) (zip format row),
            "</tr>", nl]


buildHeaderCell :: Info -> (Alignment, TableCell) -> Html
buildHeaderCell i (a, cell)
  = concat ["<th class=", quote, alignClass a, quote, ">", nl,
            buildText i cell, nl,
            "</th>", nl]


buildTableRow :: Info -> [Alignment] -> TableRow -> Html
buildTableRow i alignments row
  = concat ["<tr>", nl,
            concatMap (buildTableCell i) (zip alignments row),
            "</tr>", nl]


buildTableCell :: Info -> (Alignment, TableCell) -> Html
buildTableCell i (a, cell)
  = concat ["<td class=", quote, alignClass a, quote, ">", nl,
            buildText i cell, nl,
            "</td>", nl]


alignClass :: Alignment -> String
alignClass AlignLeft
  = "left"
alignClass AlignCenter
  = "center"
alignClass AlignRight
  = "right"
