
module DataPhrase
       where


type Phrase = [PhraseElem]

data PhraseElem =
  PeWord String |
  PeNumber String |
  PePunctuation Char |
  PeQuoteOpen |
  PeQuoteClose |
  PeDashEm |
  PeDashEn |
  PeSpace

