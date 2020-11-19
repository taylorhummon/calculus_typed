
module DataLink
       where

import DataLocation
import DataPhrase


data Link =
  LkExternal String Phrase |
  LkTarget Location |
  LkInternal Location Phrase |
  LkString Location String |
  LkFail Phrase

