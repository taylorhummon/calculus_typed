
module ParseWriting (parseWriting, parseText)
       where

import Compose
import DataWriting
import DataLocation

import Except (Excepted)
import Token (Token(..), trimSpaceToks)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, breakOnGrabLoc,
              grabFirstLoc, grabCommand)
import ParseMathLines (grabMathInLine)
import ParseLink (grabLink)
import ParsePhrase (parsePhrase)



parseWriting :: Location -> [Token] -> Excepted Writing
parseWriting loc
  = trimSpaceToks .> (parseWriting' loc)

parseWriting' :: Location -> [Token] -> Excepted Writing
parseWriting' _ []
  = return []
parseWriting' loc tokens
  = do (we, rest) <- spanWriting loc tokens
       writing <- parseWriting' loc rest
       return (we : writing)


spanWriting :: Location -> [Token] -> Excepted (WritingElem, [Token])
spanWriting loc tokens
  = caseGrabbed (grabEmphasis loc tokens)
    -- Missed
    (do (inside, rest) <- breakOnGrabLoc grabEmphasis loc tokens
        text <- parseText loc inside
        return (WePlain text, rest)
    )
    -- Grabbed we rest
    (\ we rest ->
      return (we, rest)
    )


grabEmphasis :: Location -> [Token] -> Grabbed WritingElem
grabEmphasis loc tokens
  = caseGrabbed (grabCommand "emph" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (_, args) rest ->
      do let [inside] = args
         text <- parseText loc (trimSpaceToks inside)
         return (Grabbed (WeEmphasis text) rest)
    )


parseText :: Location -> [Token] -> Excepted Text
parseText _ []
  = return []
parseText loc tokens
  = do (te, rest) <- spanText loc tokens
       text <- parseText loc rest
       return (te : text)


spanText :: Location -> [Token] -> Excepted (TextElem, [Token])
spanText loc tokens
  = caseGrabbed (grabFancyText loc tokens)
    -- Missed
    (do (inside, rest) <- breakOnGrabLoc grabFancyText loc tokens
        phrase <- parsePhrase inside
        return (TePlain phrase, rest)
    )
    -- Grabbed te rest
    (\ te rest ->
      return (te, rest)
    )


grabFancyText :: Location -> [Token] -> Grabbed TextElem
grabFancyText
  = grabFirstLoc [grabTeMath, grabTeLink]


grabTeMath :: Location -> [Token] -> Grabbed TextElem
grabTeMath _ tokens
  = caseGrabbed (grabMathInLine tokens)
    -- Missed
    (return Missed)
    -- Grabbed math rest
    (\ math rest ->
      do return (Grabbed (TeMath math) rest)
    )


grabTeLink :: Location -> [Token] -> Grabbed TextElem
grabTeLink loc tokens
  = caseGrabbed (grabLink loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed link rest
    (\ link rest ->
      return (Grabbed (TeLink link) rest)
    )


