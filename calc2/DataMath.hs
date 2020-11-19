
module DataMath
       where

import DataPhrase

type MathLines = ([MathRow], Int)
type MathRow = [MathCell]
type MathCell = (Math, Math)

type Math = [MathElem]

data MathElem =
  MeUnsafe String |
  MeText Phrase |
  MeVariable Char |
  MeNumber String |
  MeSymbol Char |
  MeSpace |
  MeCommand String [Math] |
  MeCommandOpt String (Maybe Math) [Math] |
  MeSubScript Math |
  MeSuperScript Math

