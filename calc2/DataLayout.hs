
module DataLayout
       where

import DataPassage
import DataMatter
import DataBlock
import DataWriting


type Layouts = [Block Layout]

data Layout =
  LtPlain Passages |
  LtTB EntrysTB |
  LtBF EntrysBF |
  LtTBF EntrysTBF

type EntrysTB = [Block EntryTB]
type EntrysBF = [Block EntryBF]
type EntrysTBF = [Block EntryTBF]

data EntryTB =
  EntryTB (Block Title) (Block Body)

data EntryBF =
  EntryBF (Block Body) (Block Figure)

data EntryTBF =
  EntryTBF (Block Title) (Block Body) (Block Figure)

type Title = Text

type Body = Passages

data Figure =
  FiImage (Block Image) |
  FiTable (Block Table)

