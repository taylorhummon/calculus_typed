
module WebBuildPhrase (buildPhrase)
       where

import DataPhrase

import WebCommon (Html)


buildPhrase :: Phrase -> Html
buildPhrase
  = concatMap buildPhraseElem


buildPhraseElem :: PhraseElem -> Html
buildPhraseElem (PeWord str)
  = str
buildPhraseElem (PeNumber str)
  = str
buildPhraseElem (PePunctuation c)
  = [c]
buildPhraseElem PeQuoteOpen
  = "&quot;"
buildPhraseElem PeQuoteClose
  = "&quot;"
buildPhraseElem PeDashEm
  = "&mdash;"
buildPhraseElem PeDashEn
  = "&ndash;"
buildPhraseElem PeSpace
  = " "

