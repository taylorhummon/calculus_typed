
module WebBuildWriting (buildWriting, buildText)
       where

import DataInfo
import DataWriting
import DataMath
import DataPhrase

import WebCommon (Html, bs, quote, nl)
import WebBuildMath (buildMath)
import WebBuildLink (buildLink)
import WebBuildPhrase (buildPhrase)


buildWriting :: Info -> Writing -> Html
buildWriting i writing
  = concatMap (buildWritingElem i) writing ++ nl


buildWritingElem :: Info -> WritingElem -> Html
buildWritingElem i (WePlain text)
  = buildText i text
buildWritingElem i (WeEmphasis text)
  = concat ["<em>", buildText i text, "</em>"]


buildText :: Info -> Text -> Html
buildText _ []
  = []
buildText i (TePlain pt : rest)
  = concat [buildPhrase pt,
            buildText i rest]
buildText i (TeLink link : rest)
  = concat [buildLink i link,
            buildText i rest]
buildText i (TeMath math : TePlain pt : rest)
  = case break isPeSpace pt
    of (phraseIn, phraseOut)
         -> concat [buildInlineMath i math phraseIn,
                    buildPhrase phraseOut,
                    buildText i rest]
buildText i (TeMath math : rest)
  = concat [buildInlineMath i math [],
            buildText i rest]


isPeSpace :: PhraseElem -> Bool
isPeSpace PeSpace
  = True
isPeSpace _
  = False


buildInlineMath :: Info -> Math -> Phrase -> Html
buildInlineMath _ math phrase
  = concat ["<span class=", quote, "inline", quote, ">",
            bs, "(", " ", buildMath math, " ", bs, ")",
            buildPhrase phrase,
            "</span>"]


