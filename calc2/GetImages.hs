
module GetImages (getImages)
       where

import Compose
import DataPage
import DataGroup
import DataLayout
import DataPassage
import DataMatter

import Block (unpackBlock)


{- *** Get a List of Images Included in a Page *** -}

getImages :: Page -> [String]
getImages
  = fromPage


fromPage :: Page -> [String]
fromPage (Page _ _ subpages)
  = concatMap fromSubPage subpages


fromSubPage :: SubPage -> [String]
fromSubPage (SubPage _ bgs)
  = bgs
    $> map unpackBlock
    $> map fromGroup
    $> concat


fromGroup :: Group -> [String]
fromGroup (Group _ bls)
  = bls
    $> map unpackBlock
    $> map fromLayout
    $> concat


fromLayout :: Layout -> [String]
fromLayout (LtPlain bps)
  = bps
    $> map unpackBlock
    $> map fromPassage
    $> concat
fromLayout (LtTB bes)
  = bes
    $> map unpackBlock
    $> map fromEntryTB
    $> concat
fromLayout (LtBF bes)
  = bes
    $> map unpackBlock
    $> map fromEntryBF
    $> concat
fromLayout (LtTBF bes)
  = bes
    $> map unpackBlock
    $> map fromEntryTBF
    $> concat


fromEntryTB :: EntryTB -> [String]
fromEntryTB (EntryTB _ bb)
  = bb
    $> unpackBlock
    $> fromBody


fromEntryBF :: EntryBF -> [String]
fromEntryBF (EntryBF bb bf)
  = (bb
     $> unpackBlock
     $> fromBody)
    ++
    (bf
     $> unpackBlock
     $> fromFigure)


fromEntryTBF :: EntryTBF -> [String]
fromEntryTBF (EntryTBF _ bb bf)
  = (bb
     $> unpackBlock
     $> fromBody)
    ++
    (bf
     $> unpackBlock
     $> fromFigure)


fromBody :: Body -> [String]
fromBody body
  = body
    $> map unpackBlock
    $> map fromPassage
    $> concat


fromFigure :: Figure -> [String]
fromFigure (FiImage bi)
  = fromImage (unpackBlock bi)
fromFigure _
  = []


fromPassage :: Passage -> [String]
fromPassage (PaParagraph bms)
  = bms
    $> map unpackBlock
    $> map fromMatter
    $> concat
fromPassage _
  = []


fromMatter :: Matter -> [String]
fromMatter (MaImage image)
  = fromImage image
fromMatter (MaItemize items)
  = items
    $> map unpackBlock
    $> map fromItem
    $> concat
fromMatter (MaEnumerate items)
  = items
    $> map unpackBlock
    $> map fromItem
    $> concat
fromMatter _
  = []


fromItem :: Item -> [String]
fromItem (Item matters)
  = matters
    $> map unpackBlock
    $> map fromMatter
    $> concat


fromImage :: Image -> [String]
fromImage (Image _ name)
  = [name]
