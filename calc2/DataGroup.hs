
module DataGroup
       where

import DataLayout
import DataBlock


type Groups = [Block Group]

data Group =
  Group GroupType Layouts

data GroupType =
  GtPlain |
  GtDefinition |
  GtFormula |
  GtFormulas |
  GtImportant |
  GtLaw |
  GtLaws |
  GtQuestion |
  GtTheorem |
  GtExample |
  GtWhy
  deriving (Eq)

