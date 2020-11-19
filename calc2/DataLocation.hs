
module DataLocation
       where

{-
A location is a list of page identifiers from general to specific:

  top
    or
  top, part
    or
  top, part, chapter
    or
  top, part, chapter, section

-}

type Location = [String]

