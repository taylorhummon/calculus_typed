
module WebBuildMatter (buildMatters, buildImage)
       where

import DataInfo
import DataMatter
import DataMath
import DataBlock

import WebCommon (Html, quote, bs, nl)
import WebUrls (urlImg)
import WebBuildBlock (buildTag, buildDiv)
import WebBuildWriting (buildWriting)
import WebBuildMath (buildMath')


buildMatters :: Info -> Matters -> Html
buildMatters i
  = concatMap (buildMatter i)


buildMatter :: Info -> Block Matter -> Html
buildMatter i (b @ (Block _ _ (MaPlain writing)))
  = buildDiv i "matter writing" b (buildWriting i writing)
buildMatter i (b @ (Block _ _ (MaMathLines mathLines)))
  = buildDiv i "matter math" b (buildMathLines i mathLines)
buildMatter i (b @ (Block _ _ (MaImage image)))
  = buildDiv i "matter image" b (buildImage i image)
buildMatter i (b @ (Block _ _ (MaItemize items)))
  = buildDiv i "matter itemize" b (buildItemize i items)
buildMatter i (b @ (Block _ _ (MaEnumerate items)))
  = buildDiv i "matter enumerate" b (buildEnumerate i items)


buildImage :: Info -> Image -> Html
buildImage i (Image loc file)
  = concat ["<img src=", quote, urlImg i loc file, quote, ">", nl]


buildItemize :: Info -> [Block Item] -> Html
buildItemize i items
  = concat ["<ul>", nl,
            concatMap (buildBlockItem i) items,
            "</ul>", nl]


buildEnumerate :: Info -> [Block Item] -> Html
buildEnumerate i items
  = concat ["<ol>", nl,
            concatMap (buildBlockItem i) items,
            "</ol>", nl]


buildBlockItem :: Info -> Block Item -> Html
buildBlockItem i (b @ (Block _ _ (Item matters)))
  = buildTag i "li" b (buildMatters i matters)


buildMathLines :: Info -> MathLines -> Html
buildMathLines i (mathRows, _)
  = concat [bs, "[", nl,
            bs, "begin{align}", nl,
            buildMathRows i mathRows, nl,
            bs, "end{align}", nl,
            bs, "]", nl]


buildMathRows :: Info -> [MathRow] -> Html
buildMathRows i (mathRow : [])
  = buildMathCells i mathRow
buildMathRows i (mathRow : rest)
  = concat [buildMathCells i mathRow, nl,
            bs, bs, nl,
            buildMathRows i rest]
buildMathRows _ []
  = []


buildMathCells :: Info -> [MathCell] -> Html
buildMathCells i (mathCell : [])
  = buildMathCell i mathCell
buildMathCells i (mathCell : rest)
  = concat [buildMathCell i mathCell, nl,
            amp, nl,
            buildMathCells i rest]
buildMathCells _ []
  = []


buildMathCell :: Info -> MathCell -> Html
buildMathCell _ (mathLeft, mathRight)
  = concat [buildMath' True False mathLeft,
            amp,
            buildMath' False True mathRight]


amp :: Html
amp = "&amp;"


