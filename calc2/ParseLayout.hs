
module ParseLayout (parseLayouts)
       where

import Compose
import DataLayout
import DataBlock
import DataWriting
import DataLocation

import Except (Excepted, Exception(..), throw)
import Token (Token(..), dropSpaceToks,
              trimSpaceToks, tokenException)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, breakOnGrabLoc,
              grabFirstLoc)
import ParseBlock (grabBlock, grabBlockNoHints)
import ParsePassage (parsePassages, grabTableUnadorned)
import ParseMatter (grabImageUnadorned)
import ParseWriting (parseText)
import Block (packBlock)


parseLayouts :: Location -> [Token] -> Excepted Layouts
parseLayouts loc
  = dropSpaceToks .> (parseLayouts' loc)

parseLayouts' :: Location -> [Token] -> Excepted Layouts
parseLayouts' _ []
  = return []
parseLayouts' loc tokens
  = caseGrabbed (grabExplicitLayout loc tokens)
    -- Missed
    (do (passages, rest) <- spanLayout loc tokens
        layouts <- parseLayouts loc rest
        return (passages : layouts)
    )
    -- Grabbed layout rest
    (\ layout rest ->
      do layouts <- parseLayouts loc rest
         return (layout : layouts)
    )


grabExplicitLayout :: Location -> [Token] -> Grabbed (Block Layout)
grabExplicitLayout
  = grabFirstLoc [grabLayoutPlain
                 ,grabLayoutTB
                 ,grabLayoutBF
                 ,grabLayoutTBF
                 ]


spanLayout :: Location -> [Token] -> Excepted (Block Layout, [Token])
spanLayout loc tokens
  = do (inside, rest) <- breakOnGrabLoc grabExplicitLayout loc tokens
       passages <- parsePassages loc inside
       return (packBlock (LtPlain passages), rest)


{- *** Plain *** -}

grabLayoutPlain :: Location -> [Token] -> Grabbed (Block Layout)
grabLayoutPlain loc tokens
  = grabBlock loc "layout" tokens parseLayoutPlain


parseLayoutPlain :: Location -> Int -> [Token] -> Excepted Layout
parseLayoutPlain loc _ tokens
  = do passages <- parsePassages loc tokens
       return (LtPlain passages)


{- *** titlebody *** -}

grabLayoutTB :: Location -> [Token] -> Grabbed (Block Layout)
grabLayoutTB loc tokens
  = grabBlock loc "titlebody" tokens parseLayoutTB


parseLayoutTB :: Location -> Int -> [Token] -> Excepted Layout
parseLayoutTB loc n tokens
  = do entryTBs <- parseEntrysTB loc n tokens
       return (LtTB entryTBs)


parseEntrysTB :: Location -> Int -> [Token] -> Excepted EntrysTB
parseEntrysTB loc n
  = dropSpaceToks .> (parseEntrysTB' loc n)

parseEntrysTB' :: Location -> Int -> [Token] -> Excepted EntrysTB
parseEntrysTB' _ _ []
  = return []
parseEntrysTB' loc n tokens
  = caseGrabbed (grabEntryTB loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected entry."))
    -- Grabbed blockEntry rest
    (\ be rest ->
      do bes <- parseEntrysTB loc n rest
         return (be : bes)
    )


{- *** bodyfigure *** -}

grabLayoutBF :: Location -> [Token] -> Grabbed (Block Layout)
grabLayoutBF loc tokens
  = grabBlock loc "bodyfigure" tokens parseLayoutBF


parseLayoutBF :: Location -> Int -> [Token] -> Excepted Layout
parseLayoutBF loc n tokens
  = do entryBFs <- parseEntrysBF loc n tokens
       return (LtBF entryBFs)


parseEntrysBF :: Location -> Int -> [Token] -> Excepted EntrysBF
parseEntrysBF loc n
  = dropSpaceToks .> (parseEntrysBF' loc n)

parseEntrysBF' :: Location -> Int -> [Token] -> Excepted EntrysBF
parseEntrysBF' _ _ []
  = return []
parseEntrysBF' loc n tokens
  = caseGrabbed (grabEntryBF loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected entry."))
    -- Grabbed blockEntry rest
    (\ be rest ->
      do bes <- parseEntrysBF loc n rest
         return (be : bes)
    )


{- *** titlebodyfigure *** -}

grabLayoutTBF :: Location -> [Token] -> Grabbed (Block Layout)
grabLayoutTBF loc tokens
  = grabBlock loc "titlebodyfigure" tokens parseLayoutTBF


parseLayoutTBF :: Location -> Int -> [Token] -> Excepted Layout
parseLayoutTBF loc n tokens
  = do entrysTBF <- parseEntrysTBF loc n tokens
       return (LtTBF entrysTBF)


parseEntrysTBF :: Location -> Int -> [Token] -> Excepted EntrysTBF
parseEntrysTBF loc n
  = dropSpaceToks .> (parseEntrysTBF' loc n)

parseEntrysTBF' :: Location -> Int -> [Token] -> Excepted EntrysTBF
parseEntrysTBF' _ _ []
  = return []
parseEntrysTBF' loc n tokens
  = caseGrabbed (grabEntryTBF loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected entry."))
    -- Grabbed blockEntry rest
    (\ be rest ->
      do bes <- parseEntrysTBF loc n rest
         return (be : bes)
    )


{- *** entrys *** -}

grabEntryTB :: Location -> [Token] -> Grabbed (Block EntryTB)
grabEntryTB loc tokens
  = grabBlock loc "entry" tokens parseEntryTB


parseEntryTB :: Location -> Int -> [Token] -> Excepted EntryTB
parseEntryTB loc n tokens
  = do (title, tokens') <- spanTitle loc n tokens
       (body, tokens'') <- spanBody loc n tokens'
       _ <- verifyEntryDone tokens''
       return (EntryTB title body)


grabEntryBF :: Location -> [Token] -> Grabbed (Block EntryBF)
grabEntryBF loc tokens
  = grabBlock loc "entry" tokens parseEntryBF


parseEntryBF :: Location -> Int -> [Token] -> Excepted EntryBF
parseEntryBF loc n tokens
  = do (body, tokens') <- spanBody loc n tokens
       (figure, tokens'') <- spanFigure loc n tokens'
       _ <- verifyEntryDone tokens''
       return (EntryBF body figure)


grabEntryTBF :: Location -> [Token] -> Grabbed (Block EntryTBF)
grabEntryTBF loc tokens
  = grabBlock loc "entry" tokens parseEntryTBF


parseEntryTBF :: Location -> Int -> [Token] -> Excepted EntryTBF
parseEntryTBF loc n tokens
  = do (title, tokens') <- spanTitle loc n tokens
       (body, tokens'') <- spanBody loc n tokens'
       (figure, tokens''') <- spanFigure loc n tokens''
       _ <- verifyEntryDone tokens'''
       return (EntryTBF title body figure)


verifyEntryDone :: [Token] -> Excepted ()
verifyEntryDone tokens
  = case (dropSpaceToks tokens)
    of []
         -> return ()
       (t : _)
         -> throw (tokenException t "Unexpected token in entry.")


{- *** Entrys *** -}

spanTitle :: Location -> Int -> [Token] -> Excepted (Block Text, [Token])
spanTitle loc n tokens
  = spanTitle' loc n (dropSpaceToks tokens)

spanTitle' :: Location -> Int -> [Token] -> Excepted (Block Text, [Token])
spanTitle' loc n tokens
  = caseGrabbed (grabTitle loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected title."))
    -- Grabbed blockTitle rest
    (\ bt rest ->
      do return (bt, rest)
    )


grabTitle :: Location -> [Token] -> Grabbed (Block Title)
grabTitle loc tokens
  = grabBlockNoHints loc "title" tokens parseTitle


parseTitle :: Location -> Int -> [Token] -> Excepted Title
parseTitle loc _ tokens
  = do text <- parseText loc (trimSpaceToks tokens)
       return text


spanBody :: Location -> Int -> [Token] -> Excepted (Block Body, [Token])
spanBody loc n tokens
  = spanBody' loc n (dropSpaceToks tokens)


spanBody' :: Location -> Int -> [Token] -> Excepted (Block Body, [Token])
spanBody' loc n tokens
  = caseGrabbed (grabBody loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected body."))
    -- Grabbed blockBody rest
    (\ bb rest ->
      return (bb, rest)
    )


grabBody :: Location -> [Token] -> Grabbed (Block Body)
grabBody loc tokens
  = grabBlockNoHints loc "body" tokens parseBody


parseBody :: Location -> Int -> [Token] -> Excepted Body
parseBody loc _ tokens
  = do passages <- parsePassages loc tokens
       return passages


spanFigure :: Location -> Int -> [Token] -> Excepted (Block Figure, [Token])
spanFigure loc n tokens
  = spanFigure' loc n (dropSpaceToks tokens)


spanFigure' :: Location -> Int -> [Token] -> Excepted (Block Figure, [Token])
spanFigure' loc n tokens
  = caseGrabbed (grabFigure loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected figure."))
    -- Grabbed blockFigure rest
    (\ bf rest ->
         return (bf, rest)
    )


grabFigure :: Location -> [Token] -> Grabbed (Block Figure)
grabFigure loc tokens
  = grabBlockNoHints loc "figure" tokens parseFigure


parseFigure :: Location -> Int -> [Token] -> Excepted Figure
parseFigure loc n tokens
  = tokens
    $> dropSpaceToks
    $> parseFigure' loc n

parseFigure' :: Location -> Int -> [Token] -> Excepted Figure
parseFigure' loc n tokens
  = caseGrabbed (grabFi loc tokens)
    -- Missed
    (throw (ExceptLineMessage n "Expected image or table."))
    -- Grabbed figure rest
    (\ figure rest ->
      do verifyFigureDone rest
         return figure
    )


grabFi :: Location -> [Token] -> Grabbed Figure
grabFi
  = grabFirstLoc [grabFiImage, grabFiTable]


grabFiImage :: Location -> [Token] -> Grabbed Figure
grabFiImage loc tokens
  = caseGrabbed (grabImageUnadorned False loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed blockImage rest
    (\ bi rest ->
      return (Grabbed (FiImage bi) rest)
    )


grabFiTable :: Location -> [Token] -> Grabbed Figure
grabFiTable loc tokens
  = caseGrabbed (grabTableUnadorned False loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed blockTable rest
    (\ bt rest ->
      return (Grabbed (FiTable bt) rest)
    )


verifyFigureDone :: [Token] -> Excepted ()
verifyFigureDone tokens
  = case (dropSpaceToks tokens)
    of []
         -> return ()
       (t : _)
         -> throw (tokenException t "Unexpected token in figure.")


