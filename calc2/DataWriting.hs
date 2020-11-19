
module DataWriting
       where

import DataMath
import DataLink
import DataPhrase


type Writing = [WritingElem]

data WritingElem =
  WePlain [TextElem] |
  WeEmphasis [TextElem]

type Text = [TextElem]

data TextElem =
  TePlain Phrase |
  TeMath Math |
  TeLink Link

