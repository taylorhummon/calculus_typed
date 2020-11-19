
module WebBuildLayout (buildLayouts)
       where

import Compose
import DataInfo
import DataLayout
import DataBlock

import Block (unpackBlock)
import WebCommon (Html, quote, nl)
import WebBuildPassage (buildPassages, buildTable)
import WebBuildMatter (buildImage)
import WebBuildBlock (buildDiv)
import WebBuildWriting (buildText)


buildLayouts :: Info -> Layouts -> Html
buildLayouts i
  = concatMap (buildLayout i)


buildLayout :: Info -> Block Layout -> Html
buildLayout i (b @ (Block _ _ (LtPlain passages)))
  = buildDiv i "layout plain" b (buildPassages i passages)
buildLayout i (b @ (Block _ _ (LtTB entrys)))
  = buildDiv i "layout titlebody" b (buildEntrysTB i entrys)
buildLayout i (b @ (Block _ _ (LtBF entrys)))
  = buildDiv i "layout bodyfigure" b (buildEntrysBF i entrys)
buildLayout i (b @ (Block _ _ (LtTBF entrys)))
  = buildDiv i "layout titlebodyfigure" b (buildEntrysTBF i entrys)


buildEntrysTB :: Info -> EntrysTB -> Html
buildEntrysTB i
  = concatMap (buildBlockEntryTB i)


buildBlockEntryTB :: Info -> Block EntryTB -> Html
buildBlockEntryTB i (b @ (Block _ _ entry))
  = buildDiv i "entry" b (buildEntryTB i entry)
  

buildEntryTB :: Info -> EntryTB -> Html
buildEntryTB i (EntryTB bt bb)
  = concat [buildBlockTitle i bt,
            buildBlockBody i bb]


buildEntrysBF :: Info -> EntrysBF -> Html
buildEntrysBF i
   = concatMap (buildBlockEntryBF i)


buildBlockEntryBF :: Info -> Block EntryBF -> Html
buildBlockEntryBF i (b @ (Block _ _ entry))
  = buildDiv i "entry" b (buildEntryBF i entry)
    

buildEntryBF :: Info -> EntryBF -> Html
buildEntryBF i (EntryBF bb bf)
  = concat [buildBlockBody i bb,
            buildBlockFigure i bf]


buildEntrysTBF :: Info -> EntrysTBF -> Html
buildEntrysTBF i
 = concatMap (buildBlockEntryTBF i)


buildBlockEntryTBF :: Info -> Block EntryTBF -> Html
buildBlockEntryTBF i (b @ (Block _ _ entry))
  = buildDiv i "entry" b (buildEntryTBF i entry)


buildEntryTBF :: Info -> EntryTBF -> Html
buildEntryTBF i (EntryTBF bt bb bf)
  = concat [buildBlockTitle i bt,
            "<div class=", quote, "bodyfigure", quote, ">", nl,
            buildBlockBody i bb,
            buildBlockFigure i bf,
            "</div>", nl]


buildBlockTitle :: Info -> Block Title -> Html
buildBlockTitle i (b @ (Block _ _ title)) 
  = buildDiv i "title" b (buildTitle i title)


buildTitle :: Info -> Title -> Html
buildTitle i title
  = concat ["<span class=", quote, "title", quote, ">",
            buildText i title,
            "</span>", nl]


buildBlockBody :: Info -> Block Body -> Html
buildBlockBody i (b @ (Block _ _ body))
  = buildDiv i "body" b (buildPassages i body)


buildBlockFigure :: Info -> Block Figure -> Html
buildBlockFigure i (b @ (Block _ _ figure))
  = buildDiv i "figure" b (buildFigure i figure)


buildFigure :: Info -> Figure -> Html
buildFigure i (FiImage bi)
  = bi
    $> unpackBlock
    $> buildImage i
buildFigure i (FiTable bt)
  = bt
    $> unpackBlock
    $> buildTable i


