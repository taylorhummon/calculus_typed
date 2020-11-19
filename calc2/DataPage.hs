
module DataPage
       where

import DataGroup
import DataLocation
import DataPhrase


data Page =
  Page Location String [SubPage]

data SubPage =
  SubPage (Maybe Phrase) Groups

