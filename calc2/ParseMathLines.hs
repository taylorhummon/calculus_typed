
module ParseMathLines (parseMathLines, grabMathInLine)
       where

import DataMath

import Except (Excepted, Exception(..), throw, reThrow)
import Token (Token(..))
import Parse (Grabbed, Grabbed'(..), caseGrabbed)
import ParseMath (parseMath)


-- grabbing inline math

grabMathInLine :: [Token] -> Grabbed Math
grabMathInLine tokens
  = caseGrabbed (grabMathParens tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, inside) rest
    (\ (_, inside) rest ->
      do math <- parseMath inside
         return (Grabbed math rest)
    )


grabMathParens :: [Token] -> Grabbed (Int, [Token])
grabMathParens (TnCommand n "(" : ts)
  = caseGrabbed (breakMathParens ")" [] ts
                 `reThrow` registerNoEndOfParens)
    -- Missed
    (return Missed)
    -- Grabbed inside rest
    (\ inside rest ->
      do return (Grabbed (n, inside) rest)
    )
  where
    registerNoEndOfParens :: Exception -> Exception
    registerNoEndOfParens ExceptNoEndOfParens
      = ExceptLineMessage n "Math has no end: \\(."
    registerNoEndOfParens e
      = e
grabMathParens _
  = return Missed


-- breakMathParens
-- throws ExceptNoEndOfParens

breakMathParens :: String -> [Token] -> [Token] -> Grabbed [Token]
breakMathParens _ _ []
  = throw ExceptNoEndOfParens
breakMathParens close acc ((t @ (TnCommand _ bt)) : ts)
  = if close == bt
    then return (Grabbed (reverse acc) ts)
    else breakMathParens close (t : acc) ts
breakMathParens close acc (t : ts)
  = breakMathParens close (t : acc) ts


-- parsing lines of math

parseMathLines :: Int -> [Token] -> Excepted MathLines
parseMathLines n tokens
  = do mathRows <- parseMathRows n tokens
       cols <- checkRowLens n mathRows
       return (mathRows, cols)


checkRowLens :: Int -> [MathRow] -> Excepted Int
checkRowLens n []
  = throw (ExceptLineMessage n
           "Math Lines: must have more than zero lines.")
checkRowLens n (row : rest)
  = checkRowLens' n (length row) rest


checkRowLens' :: Int -> Int -> [MathRow] -> Excepted Int
checkRowLens' _ cols []
  = return cols
checkRowLens' n cols (row : rest)
  = if cols == length row
    then checkRowLens' n cols rest
    else throw (ExceptLineMessage n
                "Math Lines: number of columns must match across rows.")


parseMathRows :: Int -> [Token] -> Excepted [MathRow]
parseMathRows _ []
  = return []
parseMathRows n tokens
  = do (inside, rest) <- breakMathRow [] tokens
       mathRow <- parseMathRow n inside
       mathRows <- parseMathRows n rest
       return (mathRow : mathRows)


breakMathRow :: [Token] -> [Token] -> Excepted ([Token], [Token])
breakMathRow acc []
  = return (reverse acc, [])
breakMathRow acc (TnCommand _ "\\" : rest)
  = return (reverse acc, rest)
breakMathRow acc (t : ts)
  = breakMathRow (t : acc) ts


parseMathRow :: Int -> [Token] -> Excepted MathRow
parseMathRow _ []
  = return []
parseMathRow n tokens
  = do (inside, rest) <- breakMathCell [] tokens
       mathCell <- parseMathCell n inside
       mathCells <- parseMathRow n rest
       return (mathCell : mathCells)


breakMathCell :: [Token] -> [Token] -> Excepted ([Token], [Token])
breakMathCell acc []
  = return (reverse acc, [])
breakMathCell acc (TnSymbol _ '|' : rest)
  = return (reverse acc, rest)
breakMathCell acc (t : ts)
  = breakMathCell (t : acc) ts


parseMathCell :: Int -> [Token] -> Excepted MathCell
parseMathCell n tokens
  = do (mathLeft, rest) <- parseMathLeft n [] tokens
       mathRight <- parseMathRight n [] rest
       return (mathLeft, mathRight)


parseMathLeft :: Int -> [Token] -> [Token] -> Excepted (Math, [Token])
parseMathLeft n _ []
  = throw (ExceptLineMessage n
           "Math Lines: each column must include an alignment character, &.")
parseMathLeft _ acc (TnSymbol _ '&' : rest)
  = do math <- parseMath (reverse acc)
       return (math, rest)
parseMathLeft n acc (t : ts)
  = parseMathLeft n (t : acc) ts


parseMathRight :: Int -> [Token] -> [Token] -> Excepted Math
parseMathRight _ acc []
  = do math <- parseMath (reverse acc)
       return math
parseMathRight n _ (TnSymbol _ '&' : _)
  = throw (ExceptLineMessage n
           "Math Lines: each column must include a unique alignment character, &.")
parseMathRight n acc (t : ts)
  = parseMathRight n (t : acc) ts
  
