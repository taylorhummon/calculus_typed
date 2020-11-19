
module ParseMath (parseMath)
       where

import DataMath

import Except (Excepted, Exception(..), throw)
import Symbols (allowedMathSymbols)
import Token (Token(..), dropSpaceToks, tokenException)
import Parse (Grabbed, Grabbed'(..), caseGrabbed, grabFirst,
              grabBrace, grabCommand, grabCommandOpt)
import ParsePhrase (parsePhrase)
import Math (commands, commandOpts)



{- *** Parsing Math *** -}

parseMath :: [Token] -> Excepted Math
parseMath (TnSpace _ : rest)
  = do restMath <- parseMath' (dropSpaceToks rest)
       return (MeSpace : restMath)
parseMath tokens
  = parseMath' tokens


parseMath' :: [Token] -> Excepted Math
parseMath' []
  = return []
parseMath' tokens
  = caseGrabbed (grabMathElem tokens)
    -- Missed
    (throw (tokenException (head tokens) "Expecting math.")
    )
    -- Grabbed me rest
    (\ me rest ->
      do restParsed <- parseMath' rest
         return (me : restParsed)
    )


grabMathElem :: [Token] -> Grabbed MathElem
grabMathElem
  = grabFirst [grabMeUnsafe
              ,grabMeText
              ,grabMeVariable
              ,grabMeNumber
              ,grabMeSymbol
              ,grabMeSpace
              ,grabMeCommand
              ,grabMeCommandOpt
              ,grabMeSubScript
              ,grabMeSuperScript
              ]


grabMeUnsafe :: [Token] -> Grabbed MathElem
grabMeUnsafe ts
  = caseGrabbed (grabCommand "unsafe" 1 ts)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (_, args) rest ->
      do let [inside] = args
             str = concatMap tokenToString inside
         return (Grabbed (MeUnsafe str) rest)
    )

tokenToString :: Token -> String
tokenToString (TnWord _ str)
  = str
tokenToString (TnNumber _ str)
  = str
tokenToString (TnSymbol _ c)
  = [c]
tokenToString (TnCommand _ str)
  = "\\" ++ str
tokenToString (TnSpace _)
  = " "


grabMeText :: [Token] -> Grabbed MathElem
grabMeText tokens
  = caseGrabbed (grabCommand "text" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (_, args) rest ->
      do let [inside] = args
         text <- parsePhrase inside
         return (Grabbed (MeText text) rest)
    )


grabMeVariable :: [Token] -> Grabbed MathElem
grabMeVariable (TnWord _ [v] : ts)
  = return (Grabbed (MeVariable v) ts)
grabMeVariable (TnWord n (v : vs) : ts)
  = return (Grabbed (MeVariable v) (TnWord n vs : ts))
grabMeVariable _
  = return Missed


grabMeNumber :: [Token] -> Grabbed MathElem
grabMeNumber (TnNumber _ str : ts)
  = return (Grabbed (MeNumber str) ts)
grabMeNumber _
  = return Missed


grabMeSymbol :: [Token] -> Grabbed MathElem
grabMeSymbol (TnSymbol _ c : ts)
  = if c `elem` allowedMathSymbols
    then return (Grabbed (MeSymbol c) ts)
    else return Missed
grabMeSymbol _
  = return Missed


grabMeSpace :: [Token] -> Grabbed MathElem
grabMeSpace (TnSpace _ : ts)
  = return (Grabbed MeSpace (dropSpaceToks ts))
grabMeSpace _
  = return Missed


grabMeCommand :: [Token] -> Grabbed MathElem
grabMeCommand (tokens @ (TnCommand _ str : _))
  = case lookup str commands
    of Just k
         -> grabMeCommand' str k tokens
       Nothing
         -> return Missed
grabMeCommand _
  = return Missed


grabMeCommand' :: String -> Int -> [Token] -> Grabbed MathElem
grabMeCommand' str nargs tokens
  = caseGrabbed (grabCommand str nargs tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, [arg]) rest
    ( \ (_, args) rest ->
       do maths <- mapM parseMath args
          return (Grabbed (MeCommand str maths) rest)
    )


grabMeCommandOpt :: [Token] -> Grabbed MathElem
grabMeCommandOpt (tokens @ (TnCommand _ str : _))
  = case lookup str commandOpts
    of Just k
         -> grabMeCommandOpt' str k tokens
       Nothing
         -> return Missed
grabMeCommandOpt _
  = return Missed


grabMeCommandOpt' :: String -> Int -> [Token] -> Grabbed MathElem
grabMeCommandOpt' str nargs tokens
  = caseGrabbed (grabCommandOpt str nargs tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, inside, [arg]) rest
    ( \ (_, inside, args) rest ->
       do maybeMath <- parseMaybeMath inside
          maths <- mapM parseMath args
          return (Grabbed (MeCommandOpt str maybeMath maths) rest)
    )


parseMaybeMath :: Maybe [Token] -> Excepted (Maybe Math)
parseMaybeMath Nothing
  = return Nothing
parseMaybeMath (Just tokens)
  = do math <- parseMath tokens
       return (Just math)


grabMeSubScript :: [Token] -> Grabbed MathElem
grabMeSubScript (TnSymbol n '_' : ts)
  = do (math, rest) <- parseMeScript n ts
       return (Grabbed (MeSubScript math) rest)
grabMeSubScript _
  = return Missed


grabMeSuperScript :: [Token] -> Grabbed MathElem
grabMeSuperScript (TnSymbol n '^' : ts)
  = do (math, rest) <- parseMeScript n ts
       return (Grabbed (MeSuperScript math) rest)
grabMeSuperScript _
  = return Missed


parseMeScript :: Int -> [Token] -> Excepted (Math, [Token])
parseMeScript n ts
  = caseGrabbed (grabBrace ts)
    -- Missed
    (caseGrabbed (grabSingle ts)
     -- Missed
     (throw (ExceptLineMessage n "Subscript / Superscript is malformed."))
     -- Grabbed token rest
     (\ token rest ->
       do math <- parseMath' [token]
          return (math, rest)
     )
    )
    -- Grabbed inside rest
    (\ inside rest ->
      do math <- parseMath' inside
         return (math, rest)
    )


-- The following function is used by superscripts and subscripts
-- to take a single token (as opposed to a {...} group).

grabSingle :: [Token] -> Grabbed Token
grabSingle (TnWord n [v] : ts)
  = return (Grabbed (TnWord n [v]) ts)
grabSingle (TnWord n (v : vs) : ts)
  = return (Grabbed (TnWord n [v]) (TnWord n vs : ts))
grabSingle (TnNumber n [d] : ts)
  = return (Grabbed (TnNumber n [d]) ts)
grabSingle (TnNumber n (d : ds) : ts)
  = return (Grabbed (TnNumber n [d]) (TnNumber n ds : ts))
grabSingle (TnSymbol n c : ts)
  = return (Grabbed (TnSymbol n c) ts)
grabSingle _
  = return Missed


