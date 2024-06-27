
module TexBuildMatter (buildMatters, buildImage)
       where

import Compose
import DataInfo
import DataMatter
import DataBlock
import DataMath

import Data.List (intersperse)
import TexCommon (Tex, bs, nl, pnl)
import TexFiles (pathFromLoc)
import TexBuildBlock (buildBlock, buildBlockSkip)
import TexBuildWriting (buildWriting)
import TexBuildMath (buildMath')


buildMatters :: Info -> Matters -> Tex
buildMatters i matters
  = matters
    $> map (buildBlockMatter i)
    $> concat


buildBlockMatter :: Info -> Block Matter -> Tex
buildBlockMatter i (b@(Block _ _ matter))
  = buildBlockSkip i b (buildMatter i matter)


buildMatter :: Info -> Matter -> Tex
buildMatter i (MaPlain w)
  = buildWriting i w
buildMatter i (MaMathLines mathlines)
  = buildMathLines i mathlines
buildMatter i (MaImage image)
  = concat [bs, "begin{center}", pnl,
            buildImage i image,
            bs, "end{center}", pnl]
buildMatter i (MaItemize items)
  = concat [bs, "begin{itemize}", pnl,
            buildItems i items,
            bs, "end{itemize}", pnl]
buildMatter i (MaEnumerate items)
  = concat [bs, "begin{enumerate}", pnl,
            buildItems i items,
            bs, "end{enumerate}", pnl]


buildImage :: Info -> Image -> Tex
buildImage _ (Image loc file)
  = concat [bs, "includegraphics[scale=1.25]{", p, "}", pnl]
  where p :: String
        p = pathFromLoc loc ++ "/" ++ file


buildItems :: Info -> [Block Item] -> Tex
buildItems i blocks
  = blocks
    $> map (buildBlockItem i)
    $> intersperse nl
    $> ([nl] ++)
    $> (++ [nl])
    $> concat


buildBlockItem :: Info -> Block Item -> Tex
buildBlockItem i (b@(Block _ _ item))
  = buildBlock i b (buildItem i item)


buildItem :: Info -> Item -> Tex
buildItem i (Item m)
  = concat [bs, "item", pnl,
            buildMatters i m]


buildMathLines :: Info -> MathLines -> Tex
buildMathLines i (mathRows, cols)
  = concat [bs, "begin{alignat*}{", intToString cols, "}", pnl,
            buildMathRows i mathRows, nl,
            bs, "end{alignat*}", pnl]

intToString :: Int -> String
intToString
  = show


buildMathRows :: Info -> [MathRow] -> Tex
buildMathRows i (mathRow : [])
  = buildMathCells i mathRow
buildMathRows i (mathRow : rest)
  = concat [buildMathCells i mathRow, nl,
            bs, bs, nl,
            buildMathRows i rest]
buildMathRows _ []
  = []


buildMathCells :: Info -> [MathCell] -> Tex
buildMathCells i (mathCell : [])
  = buildMathCell i mathCell
buildMathCells i (mathCell : rest)
  = concat [buildMathCell i mathCell, nl,
            bs, "qquad", " ", amp, nl,
            buildMathCells i rest]
buildMathCells _ []
  = []


buildMathCell :: Info -> (Math, Math) -> Tex
buildMathCell _ (mathLeft, mathRight)
  = concat [buildMath' True False mathLeft,
            amp,
            buildMath' False True mathRight]


amp :: Tex
amp = "&"
