
module UpdateLinks (updateLinks)
       where

import DataInfo
import DataPage
import DataGroup
import DataLayout
import DataPassage
import DataMatter
import DataBlock
import DataWriting
import DataLocation
import DataLink

import Block (acrossBlockIO)
import Message (messageBadLink)
import Info (validLocation, fetchPageInfo, formatTitle)



{-
This pass does two things:
1. If the location on a LkInternal does not exist, convert it to a LkFail.
2. Convert each LkTarget into a LkString or a LkFail.
For each LkFail we create, issue a warning.
-}


updateLinks :: Info -> Page -> IO Page
updateLinks info page
  = do page' <- inPage info page
       return page'


inPage :: Info -> Page -> IO Page
inPage info (Page loc title subpages)
  = do subpages' <- mapM (inSubPage info loc) subpages
       return (Page loc title subpages')


inSubPage :: Info -> Location -> SubPage -> IO SubPage
inSubPage info loc (SubPage title bgs)
  = do bgs' <- mapM (inBlockGroup info loc) bgs
       return (SubPage title bgs')


inBlockGroup :: Info -> Location -> Block Group -> IO (Block Group)
inBlockGroup info loc
  = acrossBlockIO (inGroup info loc)


inGroup :: Info -> Location -> Group -> IO Group
inGroup info loc (Group gt bls)
  = do bls' <- mapM (inBlockLayout info loc) bls
       return (Group gt bls')


inBlockLayout :: Info -> Location -> Block Layout -> IO (Block Layout)
inBlockLayout info loc
  = acrossBlockIO (inLayout info loc)


inLayout :: Info -> Location -> Layout -> IO Layout
inLayout info loc (LtPlain passages)
  = do passages' <- mapM (inBlockPassage info loc) passages
       return (LtPlain passages')
inLayout info loc (LtTB entrys)
  = do entrys' <- mapM (inBlockEntryTB info loc) entrys
       return (LtTB entrys')
inLayout info loc (LtBF entrys)
  = do entrys' <- mapM (inBlockEntryBF info loc) entrys
       return (LtBF entrys')
inLayout info loc (LtTBF entrys)
  = do entrys' <- mapM (inBlockEntryTBF info loc) entrys
       return (LtTBF entrys')


inBlockEntryTB :: Info -> Location -> Block EntryTB -> IO (Block EntryTB)
inBlockEntryTB info loc
  = acrossBlockIO (inEntryTB info loc)


inEntryTB :: Info -> Location -> EntryTB -> IO EntryTB
inEntryTB info loc (EntryTB bt bb)
  = do bt' <- inBlockTitle info loc bt
       bb' <- inBlockBody info loc bb
       return (EntryTB bt' bb')


inBlockEntryBF :: Info -> Location -> Block EntryBF -> IO (Block EntryBF)
inBlockEntryBF info loc
  = acrossBlockIO (inEntryBF info loc)


inEntryBF :: Info -> Location -> EntryBF -> IO EntryBF
inEntryBF info loc (EntryBF bb bf)
  = do bb' <- inBlockBody info loc bb
       bf' <- inBlockFigure info loc bf
       return (EntryBF bb' bf')


inBlockEntryTBF :: Info -> Location -> Block EntryTBF -> IO (Block EntryTBF)
inBlockEntryTBF info loc
  = acrossBlockIO (inEntryTBF info loc)


inEntryTBF :: Info -> Location -> EntryTBF -> IO EntryTBF
inEntryTBF info loc (EntryTBF bt bb bf)
  = do bt' <- inBlockTitle info loc bt
       bb' <- inBlockBody info loc bb
       bf' <- inBlockFigure info loc bf
       return (EntryTBF bt' bb' bf')


inBlockTitle :: Info -> Location -> Block Title -> IO (Block Title)
inBlockTitle info loc
  = acrossBlockIO (inTitle info loc)


inTitle :: Info -> Location -> Title -> IO Title
inTitle info loc title
  = mapM (inTextElem info loc) title


inBlockBody :: Info -> Location -> Block Body -> IO (Block Body)
inBlockBody info loc
  = acrossBlockIO (inBody info loc)


inBody :: Info -> Location -> Body -> IO Body
inBody info loc body
  = mapM (inBlockPassage info loc) body


inBlockFigure :: Info -> Location -> Block Figure -> IO (Block Figure)
inBlockFigure info loc
  = acrossBlockIO (inFigure info loc)


inFigure :: Info -> Location -> Figure -> IO Figure
inFigure info loc (FiTable bt)
  = do bt' <- inBlockTable info loc bt
       return (FiTable bt')
inFigure _ _ figure
  = return figure


inBlockPassage :: Info -> Location -> Block Passage -> IO (Block Passage)
inBlockPassage info loc
  = acrossBlockIO (inPassage info loc)


inPassage :: Info -> Location -> Passage -> IO Passage
inPassage info loc (PaParagraph matters)
  = do matters' <- mapM (inBlockMatter info loc) matters
       return (PaParagraph matters')
inPassage info loc (PaTable table)
  = do table' <- inTable info loc table
       return (PaTable table')


inBlockTable :: Info -> Location -> Block Table -> IO (Block Table)
inBlockTable info loc
  = acrossBlockIO (inTable info loc)


inTable :: Info -> Location -> Table -> IO Table
inTable info loc (Table len format header rows)
  = do header' <- inTableRow info loc header
       rows' <- mapM (inTableRow info loc) rows
       return (Table len format header' rows')


inTableRow :: Info -> Location -> TableRow -> IO TableRow
inTableRow info loc cells
  = mapM (inTableCell info loc) cells


inTableCell :: Info -> Location -> TableCell -> IO TableCell
inTableCell info loc cell
  = mapM (inTextElem info loc) cell


inBlockMatter :: Info -> Location -> Block Matter -> IO (Block Matter)
inBlockMatter info loc
  = acrossBlockIO (inMatter info loc)


inMatter :: Info -> Location -> Matter -> IO Matter
inMatter info loc (MaPlain writing)
  = do writing' <- mapM (inWritingElem info loc) writing
       return (MaPlain writing')
inMatter info loc (MaItemize items)
  = do items' <- mapM (inBlockItem info loc) items
       return (MaItemize items')
inMatter info loc (MaEnumerate items)
  = do items' <- mapM (inBlockItem info loc) items
       return (MaEnumerate items')
inMatter _ _ ce
  = return ce


inBlockItem :: Info -> Location -> Block Item -> IO (Block Item)
inBlockItem info loc
  = acrossBlockIO (inItem info loc)


inItem :: Info -> Location -> Item -> IO Item
inItem info loc (Item matters)
  = do matters' <- mapM (inBlockMatter info loc) matters
       return (Item matters')


inWritingElem :: Info -> Location -> WritingElem -> IO WritingElem
inWritingElem info loc (WePlain text)
  = do text' <- mapM (inTextElem info loc) text
       return (WePlain text')
inWritingElem info loc (WeEmphasis text)
  = do text' <- mapM (inTextElem info loc) text
       return (WeEmphasis text')


inTextElem :: Info -> Location -> TextElem -> IO TextElem
inTextElem info loc (TeLink link)
  = do link' <- inLink info loc link
       return (TeLink link')
inTextElem _ _ text
  = return text


inLink :: Info -> Location -> Link -> IO Link
inLink info loc (LkTarget loc')
  = if validLocation info loc'
    then do let pageInfo = fetchPageInfo info loc'
                title = formatTitle pageInfo
            return (LkString loc' title)
    else do messageBadLink loc loc'
            return (LkFail [])
inLink info loc (link @ (LkInternal loc' phrase))
  = if validLocation info loc'
    then return link
    else do messageBadLink loc loc'
            return (LkFail phrase)
inLink _ _ link
  = return link


