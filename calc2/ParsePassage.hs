
module ParsePassage (parsePassages, grabTableUnadorned)
       where

import Compose
import DataPassage
import DataBlock
import DataWriting
import DataLocation

import Except (Excepted, Exception(..), throw, reThrow)
import Token (Token(..), dropSpaceToks, trimSpaceToks, tokenException)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, grabFirstLoc,
              grabCommand)
import ParseBlock (grabBlock, grabBlockNoHints)
import ParseMatter (parseMatters)
import ParseWriting (parseText)
import Block (acrossBlock)


parsePassages :: Location -> [Token] -> Excepted Passages
parsePassages loc
  = dropSpaceToks .> (parsePassages' loc)

parsePassages' :: Location -> [Token] -> Excepted Passages
parsePassages' _ []
  = return []
parsePassages' loc tokens
  = caseGrabbed (grabPassage loc tokens)
    -- Missed
    (throw (tokenException (head tokens)
            "Expected: paragraph or table."))
    -- Grabbed passage rest
    (\ passage rest ->
      do passages <- parsePassages loc rest
         return (passage : passages)
    )


grabPassage :: Location -> [Token] -> Grabbed (Block Passage)
grabPassage
  = grabFirstLoc [grabParagraph, grabTable]


grabParagraph :: Location -> [Token] -> Grabbed (Block Passage)
grabParagraph loc tokens
  = grabBlock loc "paragraph" tokens parseParagraph


parseParagraph :: Location -> Int -> [Token] -> Excepted Passage
parseParagraph loc _ tokens
  = tokens
    $> trimSpaceToks
    $> (parseParagraph' loc)

parseParagraph' :: Location -> [Token] -> Excepted Passage
parseParagraph' loc tokens
  = do matters <- parseMatters loc tokens
       return (PaParagraph matters)


{- *** Grabbing a Table *** -}

grabTable :: Location -> [Token] -> Grabbed (Block Passage)
grabTable loc tokens
  = caseGrabbed (grabTableUnadorned True loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed blockTable rest
    (\ blockTable rest ->
      return (Grabbed (acrossBlock PaTable blockTable) rest)
    )


grabTableUnadorned :: Bool -> Location -> [Token] -> Grabbed (Block Table)
grabTableUnadorned True loc tokens
  = grabBlock loc "table" tokens parseTableUnadorned
grabTableUnadorned False loc tokens
  = grabBlockNoHints loc "table" tokens parseTableUnadorned


parseTableUnadorned :: Location -> Int -> [Token] -> Excepted Table
parseTableUnadorned loc n tokens0
  = do (tableLen, tokens1) <- spanTableLength (dropSpaceToks tokens0)
       (format, tokens2) <- spanFormat n (dropSpaceToks tokens1)
       allRows <- parseTableRows loc tokens2
       _ <- (checkRowLens (length format) allRows
             `reThrow` (registerBadTable n))
       (header, rows) <- (takeHeader allRows
                          `reThrow` (registerBadTable n))
       return (Table tableLen format header rows)


registerBadTable :: Int -> Exception -> Exception
registerBadTable n ExceptBadTable
  = ExceptLineMessage n "Could not parse table."
registerBadTable _ e
  = e


spanTableLength :: [Token] -> Excepted (TableLength, [Token])
spanTableLength tokens
  = caseGrabbed (grabCommand "long" 0 tokens)
    -- Missed
    (return (TableShort, tokens))
    -- Grabbed (n, [arg]) rest
    (\ _ rest ->
      return (TableLong, rest)
    )


spanFormat :: Int -> [Token] -> Excepted ([Alignment], [Token])
spanFormat n tokens
  = caseGrabbed (grabCommand "format" 1 tokens)
    -- Missed
    (throw (ExceptLineMessage n
            "Table requires a format command."))
    -- Grabbed (n, [arg]) rest
    (\ (_, inside) rest ->
      case inside
      of [[TnWord _ str]]
           -> do aligns <- mapM (charToAlignment n) str
                 return (aligns, rest)
         _
           -> throw (ExceptLineMessage n
                     "Could not parse table format string.")
    )


charToAlignment :: Int -> Char -> Excepted Alignment
charToAlignment _ 'l'
  = return AlignLeft
charToAlignment _ 'c'
  = return AlignCenter
charToAlignment _ 'r'
  = return AlignRight
charToAlignment n _
  = throw (ExceptLineMessage n
           "Could not parse table alignment string.")


-- checkRowLens Throws ExceptBadTable.
checkRowLens :: Int -> [TableRow] -> Excepted [()]
checkRowLens len
  = mapM (checkRowLen len)


-- checkRowLen Throws ExceptBadTable.
checkRowLen :: Int -> TableRow -> Excepted ()
checkRowLen len row
  = if length row == len
    then return ()
    else throw ExceptBadTable


-- takeHeader Throws ExceptBadTable.
takeHeader :: [TableRow] -> Excepted (TableRow, [TableRow])
takeHeader []
  = throw ExceptBadTable
takeHeader (header : rows)
  = return (header, rows)


parseTableRows :: Location -> [Token] -> Excepted [TableRow]
parseTableRows loc
  = dropSpaceToks .> (parseTableRows' loc)


parseTableRows' :: Location -> [Token] -> Excepted [TableRow]
parseTableRows' _ []
  = return []
parseTableRows' loc tokens
  = do (row, rest) <- breakTableRow loc [] tokens
       cells <- parseTableRow loc row
       rows <- parseTableRows' loc rest
       case cells
         of []
              -> return rows              -- ignore empty rows
            _
              -> return (cells : rows)


breakTableRow :: Location -> [Token] -> [Token] -> Excepted ([Token], [Token])
breakTableRow _ acc []
  = return (reverse acc, [])
breakTableRow _ acc (TnCommand _ "row" : ts)
  = return (reverse acc, ts)
breakTableRow loc acc (t : ts)
  = breakTableRow loc (t : acc) ts


parseTableRow :: Location -> [Token] -> Excepted TableRow
parseTableRow loc
  = dropSpaceToks .> (parseTableRow' loc)

parseTableRow' :: Location -> [Token] -> Excepted TableRow
parseTableRow' _ []
  = return []
parseTableRow' loc tokens
  = caseGrabbed (grabTableCell loc tokens)
    -- Missed
    (throw (tokenException (head tokens) "Could not parse table."))
    -- Grabbed writing rest
    (\ cell rest ->
      do cells <- parseTableRow loc rest
         return (cell : cells)
    )


grabTableCell :: Location -> [Token] -> Grabbed Text
grabTableCell loc tokens
  = caseGrabbed (grabCommand "cell" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (_, args) rest ->
      do let [inside] = args
         text <- parseText loc inside
         return (Grabbed text rest)
    )

