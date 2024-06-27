
module UpdateMath (updateMath)
       where

import DataPage
import DataGroup
import DataLayout
import DataPassage
import DataMatter
import DataBlock
import DataWriting
import DataMath
import DataInfo

import Block (acrossBlock)
import Symbols (transformSymbol)
import MathTransform (transformCommand, transformCommandOpt)


updateMath :: Info -> Page -> IO Page
updateMath _ page
  = do let page' = inPage page
       return page'


inPage :: Page -> Page
inPage (Page loc title subpages)
  = Page
    loc
    title
    (map inSubPage subpages)


inSubPage :: SubPage -> SubPage
inSubPage (SubPage title bgs)
  = SubPage
    title
    (map inBlockGroup bgs)


inBlockGroup :: Block Group -> Block Group
inBlockGroup
  = acrossBlock inGroup


inGroup :: Group -> Group
inGroup (Group gt bls)
  = Group
    gt
    (map inBlockLayout bls)


inBlockLayout :: Block Layout -> Block Layout
inBlockLayout
  = acrossBlock inLayout


inLayout :: Layout -> Layout
inLayout (LtPlain bps)
  = LtPlain
    (map inBlockPassage bps)
inLayout (LtTB entrys)
  = LtTB
    (map inBlockEntryTB entrys)
inLayout (LtBF entrys)
  = LtBF
    (map inBlockEntryBF entrys)
inLayout (LtTBF entrys)
  = LtTBF
    (map inBlockEntryTBF entrys)


inBlockEntryTB :: Block EntryTB -> Block EntryTB
inBlockEntryTB
  = acrossBlock inEntryTB


inEntryTB :: EntryTB -> EntryTB
inEntryTB (EntryTB bt bb)
  = EntryTB
    (inBlockTitle bt)
    (inBlockBody bb)


inBlockEntryBF :: Block EntryBF -> Block EntryBF
inBlockEntryBF
  = acrossBlock inEntryBF


inEntryBF :: EntryBF -> EntryBF
inEntryBF (EntryBF bb bf)
  = EntryBF
    (inBlockBody bb)
    (inBlockFigure bf)


inBlockEntryTBF :: Block EntryTBF -> Block EntryTBF
inBlockEntryTBF
  = acrossBlock inEntryTBF


inEntryTBF :: EntryTBF -> EntryTBF
inEntryTBF (EntryTBF bt bb bf)
  = EntryTBF
    (inBlockTitle bt)
    (inBlockBody bb)
    (inBlockFigure bf)


inBlockTitle :: Block Title -> Block Title
inBlockTitle
  = acrossBlock inTitle


inTitle :: Title -> Title
inTitle title
  = map inTextElem title


inBlockBody :: Block Body -> Block Body
inBlockBody
  = acrossBlock inBody


inBody :: Body -> Body
inBody body
  = map inBlockPassage body


inBlockFigure :: Block Figure -> Block Figure
inBlockFigure
  = acrossBlock inFigure


inFigure :: Figure -> Figure
inFigure (FiTable bt)
  = FiTable (inBlockTable bt)
inFigure figure
  = figure


inBlockPassage :: Block Passage -> Block Passage
inBlockPassage
  = acrossBlock inPassage


inPassage :: Passage -> Passage
inPassage (PaParagraph matters)
  = PaParagraph (map inBlockMatter matters)
inPassage (PaTable table)
  = PaTable (inTable table)


inBlockTable :: Block Table -> Block Table
inBlockTable
  = acrossBlock inTable


inTable :: Table -> Table
inTable (Table len format header rows)
  = Table
    len
    format
    (inTableRow header)
    (map inTableRow rows)


inTableRow :: TableRow -> TableRow
inTableRow cells
  = map inTableCell cells


inTableCell :: TableCell -> TableCell
inTableCell cell
  = map inTextElem cell


inBlockMatter :: Block Matter -> Block Matter
inBlockMatter
  = acrossBlock inMatter


inMatter :: Matter -> Matter
inMatter (MaPlain writing)
  = MaPlain
    (map inWritingElem writing)
inMatter (MaMathLines mathlines)
  = MaMathLines
    (inMathLines mathlines)
inMatter (MaItemize items)
  = MaItemize (map inBlockItem items)
inMatter (MaEnumerate items)
  = MaEnumerate (map inBlockItem items)
inMatter matter
  = matter


inMathLines :: MathLines -> MathLines
inMathLines (mathRows, cols)
  = (map inMathCells mathRows, cols)


inMathCells :: [MathCell] -> [MathCell]
inMathCells mathRow
  = map inMathCell mathRow


inMathCell :: MathCell -> MathCell
inMathCell (mathLeft, mathRight)
  = (inMath mathLeft, inMath mathRight)


inBlockItem :: Block Item -> Block Item
inBlockItem
  = acrossBlock inItem


inItem :: Item -> Item
inItem (Item matters)
  = Item
    (map inBlockMatter matters)


inWritingElem :: WritingElem -> WritingElem
inWritingElem (WePlain text)
 = WePlain
   (map inTextElem text)
inWritingElem (WeEmphasis text)
  = WeEmphasis
    (map inTextElem text)


inTextElem :: TextElem -> TextElem
inTextElem (TeMath math)
  = TeMath (inMath math)
inTextElem text
  = text


inMath :: Math -> Math
inMath
  = concatMap inMathElem


inMathElem :: MathElem -> Math
inMathElem (me@(MeVariable _))
  = [me]

inMathElem (me@(MeNumber _))
  = [me]

inMathElem (MeSymbol c)
  = transformSymbol c

inMathElem (me@MeSpace)
  = [me]

inMathElem (me@(MeText _))
  = [me]

inMathElem (me@(MeUnsafe _))
  = [me]

inMathElem (MeSubScript math)
  = [MeSubScript (inMath math)]

inMathElem (MeSuperScript math)
  = [MeSuperScript (inMath math)]

inMathElem (MeCommand str maths)
  = transformCommand str (map inMath maths)

inMathElem (MeCommandOpt str maybeMath maths)
  = transformCommandOpt str maybeMath' (map inMath maths)
    where maybeMath' = case maybeMath
                       of Nothing   -> Nothing
                          Just math -> Just (inMath math)
