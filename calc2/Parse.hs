{-
Module Parse provides grab functions that to be used in parsing.

A grab function takes a list of tokens and check for
some abstract page element.
If it finds the element, it produces:
 1. the information associated with that element, and
 2. a "rest of tokens".
Otherwise, it reports that it Missed.

A grabber may throw an exception provided:
 1. it finds enough of a match to ensure that it
    is the only grabber that will match, and
 2. it cannot produce a well-formed element from the
    given input tokens.
-}

module Parse (Grabbed
             ,Grabbed'(..)
             ,BeginEndArgs
             ,caseGrabbed
             ,grabFirst
             ,grabFirstLoc
             ,breakOnGrab
             ,breakOnGrabLoc
             ,grabCommand
             ,grabCommandOpt
             ,grabBeginEnd
             ,grabBrace
             )
       where

import Compose
import DataLocation

import Except (Excepted, Exception(..), throw, reThrow)
import Token (Token(..), dropSpaceToks)

import Control.Monad (liftM)


type Grabbed a =
  Excepted (Grabbed' a)

data Grabbed' a =
  Missed |
  Grabbed a [Token]

type BeginEndArgs =
  (Maybe [Token], Maybe [Token])


{- *** Pull Information Out of a Grabbed Value *** -}

caseGrabbed :: Grabbed a ->
               Excepted b ->
               (a -> [Token] -> Excepted b) ->
               Excepted b
caseGrabbed x y f
  = do x' <- x
       case x'
         of Missed
              -> y
            Grabbed x'' rest
              -> f x'' rest



{- *** Combine Grabbers *** -}

grabFirst :: [[Token] -> Grabbed a] ->
             [Token] -> Grabbed a
grabFirst grabbers tokens
  = grabbers
    $> mapM (\ g -> g tokens)
    $> liftM firstGrabbed


grabFirstLoc :: [Location -> [Token] -> Grabbed a] ->
                Location -> [Token] -> Grabbed a
grabFirstLoc grabbers loc tokens
  = grabbers
    $> mapM (\ g -> g loc tokens)
    $> liftM firstGrabbed


firstGrabbed :: [Grabbed' a] -> Grabbed' a
firstGrabbed []
  = Missed
firstGrabbed (Grabbed x rest : _)
  = Grabbed x rest
firstGrabbed (Missed : ts)
  = firstGrabbed ts



{- *** Use a Grabber to Break tokens *** -}

breakOnGrab  :: ([Token] -> Grabbed a) ->
                [Token] -> Excepted ([Token], [Token])
breakOnGrab grabber tokens
  = breakOnGrab' grabber [] tokens

breakOnGrab' :: ([Token] -> Grabbed a) ->
                [Token] -> [Token] -> Excepted ([Token], [Token])
breakOnGrab' _ acc []
  = return (reverse acc, [])
breakOnGrab' grabber acc (tokens@(t : ts))
  = caseGrabbed (grabber tokens)
    -- Missed
    (breakOnGrab' grabber (t : acc) ts)
    -- Grabbed obj rest
    (\ _ _ ->
      return (reverse acc, tokens)
    )


breakOnGrabLoc  :: (Location -> [Token] -> Grabbed a) ->
                   Location -> [Token] -> Excepted ([Token], [Token])
breakOnGrabLoc grabber loc tokens
  = breakOnGrabLoc' grabber loc [] tokens

breakOnGrabLoc' :: (Location -> [Token] -> Grabbed a) ->
                   Location -> [Token] -> [Token] -> Excepted ([Token], [Token])
breakOnGrabLoc' _ _ acc []
  = return (reverse acc, [])
breakOnGrabLoc' grabber loc acc (tokens@(t : ts))
  = caseGrabbed (grabber loc tokens)
    -- Missed
    (breakOnGrabLoc' grabber loc (t : acc) ts)
    -- Grabbed obj rest
    (\ _ _ ->
      return (reverse acc, tokens)
    )



{- *** Grabbing Commands *** -}

-- grabs a command with n arguments
-- eg. \str{...}{...} ...
-- produces: Grabbed (n, [arg]) rest

grabCommand :: String -> Int -> [Token] -> Grabbed (Int, [[Token]])
grabCommand str nargs (TnCommand n str' : ts)
  = if str == str'
    then do (toksList, rest) <- parseBraces nargs ts
                                `reThrow` registerBadBraces n str
            return (Grabbed (n, toksList) rest)
    else return Missed
grabCommand _ _ _
  = return Missed


-- grabs a command with n arguments, and an optional argument
-- eg. \str[...]{...}{...} ...
-- produces: Grabbed (n, maybeToks, [arg]) rest

grabCommandOpt :: String -> Int -> [Token] -> Grabbed (Int, Maybe [Token], [[Token]])
grabCommandOpt str nargs (TnCommand n str' : ts)
  = if str == str'
    then caseGrabbed (grabBracket ts)
         -- Missed
         (do (toksList, rest') <- parseBraces nargs ts
                                  `reThrow` registerBadBraces n str
             return (Grabbed (n, Nothing, toksList) rest')
         )
         -- Grabbed inside rest
         (\ inside rest ->
           do (toksList, rest') <- parseBraces nargs rest
                                   `reThrow` registerBadBraces n str
              return (Grabbed (n, Just inside, toksList) rest')
         )
    else return Missed
grabCommandOpt _ _ _
  = return Missed


registerBadBraces :: Int -> String -> Exception -> Exception
registerBadBraces n str ExceptBadBraces
  = ExceptLineMessage n ("Could not parse arguments for command, " ++ str)
registerBadBraces _ _ e
  = e


-- parseBraces
-- throws ExceptBadBraces

parseBraces :: Int -> [Token] -> Excepted ([[Token]], [Token])
parseBraces 0 tokens
  = return ([], tokens)
parseBraces nargs tokens
  = parseBraces' nargs (dropSpaceToks tokens)

parseBraces' :: Int -> [Token] -> Excepted ([[Token]], [Token])
parseBraces' nargs tokens
  = caseGrabbed (grabBrace tokens)
    -- Missed
    (throw ExceptBadBraces)
    -- Grabbed inside rest
    (\ inside rest ->
      do (inside', rest') <- parseBraces (nargs - 1) rest
         return (inside : inside', rest')
    )


-- grabs a begin token, and an optional argument
-- eg. \begin[...]{...} ...
-- produces: Grabbed (n, maybeToks) rest

grabBegin :: String -> [Token] -> Grabbed (Int, Maybe [Token])
grabBegin str tokens
  = caseGrabbed (grabCommandOpt "begin" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, maybeToks, inside) rest
    (\ (n, maybeToks, inside) rest ->
      case inside
      of [[TnWord _ str']]
           -> if str == str'
              then return (Grabbed (n, maybeToks) rest)
              else return Missed
         _
           -> throw (ExceptLineMessage n
                     "Could not parse arguments of command: begin.")
    )


-- grabs an end token
-- eg. \end[...]{...} ...
-- produces: Grabbed (n, maybeToks) rest

grabEnd :: String -> [Token] -> Grabbed (Int, Maybe [Token])
grabEnd str tokens
  = caseGrabbed (grabCommandOpt "end" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, maybeToks, inside) rest
    (\ (n, maybeToks, inside) rest ->
      case inside
      of [[TnWord _ str']]
           -> if str == str'
              then return (Grabbed (n, maybeToks) rest)
              else return Missed
         _
           -> throw (ExceptLineMessage n
                     "Could not parse arguments of command: end.")
    )


-- grabs a begin/end pair, and an optional argument
-- eg. \begin{example} ... \end{example} ...
-- eg. \begin{enumerate} ... \end{enumerate} ...
-- eg. \begin{table} ... \end{table} ...
-- produces: Grabbed (n, maybeToks, inside) rest

grabBeginEnd :: String -> [Token] -> Grabbed (Int, BeginEndArgs, [Token])
grabBeginEnd str tokens
  = caseGrabbed (grabBegin str tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, maybeToks) rest
    (\ (n, beginArgs) rest ->
      do (endArgs, inside, rest') <- (spanBeginEnd str [] rest)
                                     `reThrow` (registerNoEndOfParens str n)
         return (Grabbed (n, (beginArgs, endArgs), inside) rest')
    )
  where registerNoEndOfParens :: String -> Int -> Exception -> Exception
        registerNoEndOfParens str' n ExceptNoEndOfParens
          = ExceptLineMessage n (concat ["Could not find end to: ",
                                         "begin", "{", str', "}", "."])
        registerNoEndOfParens _ _ e
          = e


-- spanBeginEnd
-- throws NoEndOfParens

spanBeginEnd :: String -> [Token] -> [Token] -> Excepted (Maybe [Token], [Token], [Token])
spanBeginEnd _ _ []
  = throw (ExceptNoEndOfParens)
spanBeginEnd str acc (tokens@(t : ts))
  = caseGrabbed (grabEnd str tokens)
     -- Missed
     (spanBeginEnd str (t : acc) ts)
     -- Grabbed (n, maybeToks) rest
     (\ (_, maybeToks) rest ->
       return (maybeToks, reverse acc, rest)
     )


-- grabs a brace:
-- eg. { ... } ...
-- produces: Grabbed inside rest

grabBrace :: [Token] -> Grabbed [Token]
grabBrace (TnSymbol n '{' : ts)
  = grabParens ('{', '}') 1 [] ts
    `reThrow` registerNoEndOfParens
  where
    registerNoEndOfParens :: Exception -> Exception
    registerNoEndOfParens ExceptNoEndOfParens
      = ExceptLineMessage n "Could not find end to: {."
    registerNoEndOfParens e
      = e
grabBrace _
  = return Missed


-- grabs a bracket
-- eg. [ ... ] ...
-- produces: Grabbed inside rest

grabBracket :: [Token] -> Grabbed [Token]
grabBracket (TnSymbol n '[' : ts)
  = grabParens ('[', ']') 1 [] ts
    `reThrow` registerNoEndOfParens
  where
    registerNoEndOfParens :: Exception -> Exception
    registerNoEndOfParens ExceptNoEndOfParens
      = ExceptLineMessage n "Could not find end to: ["
    registerNoEndOfParens e
      = e
grabBracket _
  = return Missed


-- grabParens
-- throws ExceptNoEndOfParens

grabParens :: (Char, Char) -> Integer -> [Token] -> [Token]
              -> Grabbed [Token]
grabParens _ 0 (_ : acc) tokens
  = return (Grabbed (reverse acc) tokens)
grabParens _ _ _ []
  = throw ExceptNoEndOfParens
grabParens (open, close) depth acc (t@(TnSymbol _ bt) : ts)
  | bt == open   = grabParens (open, close) (depth + 1) (t : acc) ts
  | bt == close  = grabParens (open, close) (depth - 1) (t : acc) ts
  | otherwise    = grabParens (open, close) depth       (t : acc) ts
grabParens (open, close) depth acc (t : ts)
  = grabParens (open, close) depth (t : acc) ts
