
module ParsePhrase (parsePhrase)
       where

import DataPhrase

import Except (Excepted, throw)
import Token (Token(..), tokenException)
import Parse (Grabbed, Grabbed'(..), caseGrabbed)
import Symbols (allowedPhraseSymbols)


parsePhrase :: [Token] -> Excepted Phrase
parsePhrase []
  = return []
parsePhrase tokens
  = caseGrabbed (grabPhraseElem tokens)
    -- Missed
    (throw (tokenException (head tokens) "Expecting phrase."))
    -- Grabbed pe rest
    (\ pe rest ->
      do pes <- parsePhrase rest
         return (pe : pes)
    )


grabPhraseElem :: [Token] -> Grabbed PhraseElem
grabPhraseElem (TnWord _ str : ts)
  = return (Grabbed (PeWord str) ts)
grabPhraseElem (TnNumber _ str : ts)
  = return (Grabbed (PeNumber str) ts)
grabPhraseElem (tokens@(TnSymbol _ _ : _))
  = grabPunctuation tokens
grabPhraseElem (TnSpace _ : ts)
  = return (Grabbed PeSpace ts)
grabPhraseElem _
  = return Missed


grabPunctuation :: [Token] -> Grabbed PhraseElem
grabPunctuation (TnSymbol _ '`' : TnSymbol _ '`' : ts)
  = return (Grabbed PeQuoteOpen ts)
grabPunctuation (TnSymbol _ '\'' : TnSymbol _ '\'' : ts)
  = return (Grabbed PeQuoteClose ts)
grabPunctuation (TnSymbol _ '-' : TnSymbol _ '-' : TnSymbol _ '-' : ts)
  = return (Grabbed PeDashEm ts)
grabPunctuation (TnSymbol _ '-' : TnSymbol _ '-' : ts)
  = return (Grabbed PeDashEn ts)
grabPunctuation (TnSymbol _ symb : ts)
  | elem symb allowedPhraseSymbols
    = return (Grabbed (PePunctuation symb) ts)
  | otherwise
    = return Missed
grabPunctuation _
  = return Missed
