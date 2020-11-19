
module Symbols (allowedSymbols
               ,allowedPhraseSymbols
               ,allowedMathSymbols
               ,transformSymbol)
       where

import DataMath


allowedSymbols :: [Char]
allowedSymbols
  = punctuation ++ mathSymbols ++ ['[', ']', '&', '|', '\\']


allowedPhraseSymbols :: [Char]
allowedPhraseSymbols
  = punctuation


allowedMathSymbols :: [Char]
allowedMathSymbols
  = punctuation ++ mathSymbols


punctuation :: [Char]
punctuation
  = ".,;:!?-()" ++ ['\'', '`']


mathSymbols :: [Char]
mathSymbols
  = "+-/=<>(){}_^"


transformSymbol :: Char -> Math
transformSymbol '<'
  = [MeCommand "lt" []]
transformSymbol '>'
  = [MeCommand "gt" []]
transformSymbol c
  = [MeSymbol c]

