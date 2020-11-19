
module TexBuildPhrase (buildPhrase)
       where

import DataPhrase

import TexCommon (Tex)


buildPhrase :: Phrase -> Tex
buildPhrase
  = concatMap buildPhraseElem

buildPhraseElem :: PhraseElem -> Tex
buildPhraseElem (PeWord str)
  = str
buildPhraseElem (PeNumber str)
  = str
buildPhraseElem (PePunctuation c)
  = [c]
buildPhraseElem PeQuoteOpen
  = "``"
buildPhraseElem PeQuoteClose
  = "''"
buildPhraseElem PeDashEm
  = "---"
buildPhraseElem PeDashEn
  = "--"
buildPhraseElem PeSpace
  = " "

