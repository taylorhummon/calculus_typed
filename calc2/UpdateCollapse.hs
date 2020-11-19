
module UpdateCollapse (updateCollapse)
       where

import Compose
import DataInfo
import DataPage
import DataGroup
import DataLayout
import DataPassage
import DataMatter
import DataBlock

import Block (acrossBlock)


updateBlocks :: [Block a] -> [Block a]
updateBlocks
  = collapseFirst .> collapseLast


collapseFirst :: [Block a] -> [Block a]
collapseFirst []
  = []
collapseFirst (Block hints (_, b2) thing : rest)
  = Block hints (True, b2) thing : rest


collapseLast :: [Block a] -> [Block a]
collapseLast []
  = []
collapseLast (Block hints (b1, _) thing : [])
  = Block hints (b1, True) thing : []
collapseLast (block : rest)
  = block : collapseLast rest


{- *** Updating a Page *** -}

updateCollapse :: Info -> Page -> IO Page
updateCollapse _ page
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
    (inBlockGroups bgs)


inBlockGroups :: [Block Group] -> [Block Group]
inBlockGroups
  = map (acrossBlock inGroup)
    .> updateBlocks


inGroup :: Group -> Group
inGroup (Group gt bls)
  = Group
    gt
    (inBlockLayouts bls)


inBlockLayouts :: [Block Layout] -> [Block Layout]
inBlockLayouts
  = map (acrossBlock inLayout)
    .> updateBlocks


inLayout :: Layout -> Layout
inLayout (LtPlain bps)
  = LtPlain
    (inBlockPassages bps)
inLayout (LtTB entrys)
  = LtTB
    (inBlockEntryTBs entrys)
inLayout (LtBF entrys)
  = LtBF
    (inBlockEntryBFs entrys)
inLayout (LtTBF entrys)
  = LtTBF
    (inBlockEntryTBFs entrys)


inBlockEntryTBs :: [Block EntryTB] -> [Block EntryTB]
inBlockEntryTBs
  = map (acrossBlock inEntryTB)
    .> updateBlocks


inEntryTB :: EntryTB -> EntryTB
inEntryTB (EntryTB bt bb)
  = EntryTB
    bt
    (inBlockBody bb)


inBlockEntryBFs :: [Block EntryBF] -> [Block EntryBF]
inBlockEntryBFs
  = map (acrossBlock inEntryBF)
    .> updateBlocks


inEntryBF :: EntryBF -> EntryBF
inEntryBF (EntryBF bb bf)
  = EntryBF
    (inBlockBody bb)
    bf


inBlockEntryTBFs :: [Block EntryTBF] -> [Block EntryTBF]
inBlockEntryTBFs
  = map (acrossBlock inEntryTBF)
    .> updateBlocks


inEntryTBF :: EntryTBF -> EntryTBF
inEntryTBF (EntryTBF bt bb bf)
  = EntryTBF
    bt
    (inBlockBody bb)
    bf


inBlockBody :: Block Body -> Block Body
inBlockBody
  = acrossBlock inBody


inBody :: Body -> Body
inBody
  = inBlockPassages


inBlockPassages :: [Block Passage] -> [Block Passage]
inBlockPassages
  = map (acrossBlock inPassage)
    .> updateBlocks


inPassage :: Passage -> Passage
inPassage (PaParagraph matters)
  = PaParagraph (inBlockMatters matters)
inPassage passage
  = passage


inBlockMatters :: [Block Matter] -> [Block Matter]
inBlockMatters
  = map (acrossBlock inMatter)
    .> updateBlocks


inMatter :: Matter -> Matter
inMatter (MaItemize items)
  = MaItemize (inBlockItems items)
inMatter (MaEnumerate items)
  = MaEnumerate (inBlockItems items)
inMatter matter
  = matter


inBlockItems :: [Block Item] -> [Block Item]
inBlockItems
  = map (acrossBlock inItem)
    .> updateBlocks


inItem :: Item -> Item
inItem (Item matters)
  = Item (inBlockMatters matters)

