
module TexBuildWriting (buildWriting, buildText)
       where

import Compose
import DataInfo
import DataWriting

import TexCommon (Tex, bs, nl)
import TexBuildMath (buildMath)
import TexBuildLink (buildLink)
import TexBuildPhrase (buildPhrase)


buildWriting :: Info -> Writing -> Tex
buildWriting i writing
  = writing
    $> map (buildWritingElem i)
    $> (++ [nl])
    $> concat


buildWritingElem :: Info -> WritingElem -> Tex
buildWritingElem i (WePlain text)
  = buildText i text
buildWritingElem i (WeEmphasis text)
  = concat [bs, "emph{", buildText i text, "}"]


buildText :: Info -> Text -> Tex
buildText i
  = concatMap (buildTextElem i)


buildTextElem :: Info -> TextElem -> Tex
buildTextElem _ (TePlain pt)
  = buildPhrase pt
buildTextElem _ (TeMath math)
  = concat ["\\( ", buildMath math, " \\)"]
buildTextElem i (TeLink link)
  = concat [buildLink i link]


