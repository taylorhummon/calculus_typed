
module DataPassage
       where

import DataMatter
import DataBlock
import DataWriting


type Passages = [Block Passage]

data Passage =
  PaParagraph Matters |
  PaTable Table

data Table =
  Table TableLength TableFormat TableRow [TableRow]

data TableLength =
  TableShort |
  TableLong

type TableFormat = [Alignment]

data Alignment =
  AlignLeft | AlignCenter | AlignRight

type TableRow = [TableCell]
type TableCell = Text

