
module ParseLink (grabLink)
       where

import Compose
import DataLink
import DataLocation

import Token (Token(..), dropSpaceToks, trimSpaceToks, tokenException)
import Except (Excepted, Exception(..), throw, reThrow)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, grabFirstLoc, grabCommand)
import ParsePhrase (parsePhrase)




{- *** Grabbing Links *** -}

grabLink :: Location -> [Token] -> Grabbed Link
grabLink
  = grabFirstLoc [grabLkExternal, grabLkTarget, grabLkInternal]



{- *** Grabbing External Links *** -}

grabLkExternal :: Location -> [Token] -> Grabbed Link
grabLkExternal loc tokens
  = caseGrabbed (grabCommand "href" 2 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (n, args) rest ->
      do let [inside, inside'] = args
         extLink <- (parseExternalLink loc inside inside'
                     `reThrow` registerBadUrl n)
         return (Grabbed extLink rest)
    )


registerBadUrl :: Int -> Exception -> Exception
registerBadUrl n ExceptBadUrl
  = ExceptLineMessage n "Could not parse external link URL."
registerBadUrl _ e
  = e


-- parseExternalLink
-- throws ExceptBadUrl

parseExternalLink :: Location -> [Token] -> [Token] -> Excepted Link
parseExternalLink _ urlToks textToks
  = do url <- parseUrl urlToks
       pt <- parsePhrase textToks
       return (LkExternal url pt)


parseUrl :: [Token] -> Excepted String
parseUrl []
  = return []
parseUrl (TnWord _ str : ts)
  = do parsed <- parseUrl ts
       return (str ++ parsed)
parseUrl (TnNumber _ str : ts)
  = do parsed <- parseUrl ts
       return (str ++ parsed)
parseUrl (TnSymbol _ c : ts)
  = do parsed <- parseUrl ts
       return (c : parsed)
parseUrl _
  = throw ExceptBadUrl



{- *** Grabbing Target and Internal Links *** -}

grabLkTarget :: Location -> [Token] -> Grabbed Link
grabLkTarget loc tokens
  = caseGrabbed (grabCommand "target" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (n, args) rest ->
      do let [inside] = args
         loc' <- parseLoc loc n inside
         return (Grabbed (LkTarget loc') rest)
    )


grabLkInternal :: Location -> [Token] -> Grabbed Link
grabLkInternal loc tokens
  = caseGrabbed (grabCommand "link" 2 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (n, args) rest ->
      do let [inside, inside'] = args
         loc' <- parseLoc loc n inside
         phrase <- parsePhrase inside'
         return (Grabbed (LkInternal loc' phrase) rest)
    )


parseLoc :: Location -> Int -> [Token] -> Excepted Location
parseLoc loc n tokens
  = do kvs <- parseKeyVals tokens
       onlyAllowed n ["book", "part", "chapter", "section"] kvs
       idBook <- select n "book" kvs
       idPart <- select n "part" kvs
       idChap <- select n "chapter" kvs
       idSect <- select n "section" kvs
       return (updateLoc loc [idBook, idPart, idChap, idSect])


updateLoc :: Location -> [Maybe String] -> Location
updateLoc loc changes
  = updateLoc' False loc changes

updateLoc' :: Bool -> Location -> [Maybe String] -> Location
updateLoc' _ _ []
  = []
updateLoc' _ (_ : rest) (Just pageId : rest')
  = pageId : updateLoc' True rest rest'
updateLoc' False (pageId : rest) (Nothing : rest')
  = pageId : updateLoc' False rest rest'
updateLoc' True (_ : _) (Nothing : _)
  = []
updateLoc' _ [] (Just pageId : rest')
  = pageId : updateLoc' True [] rest'
updateLoc' _ [] (Nothing : _)
  = []


onlyAllowed :: Int -> [String] -> [(String, String)] -> Excepted ()
onlyAllowed n strs kvs
  = let unknowns = kvs
                   $> map fst
                   $> filter ((`elem` strs) .> not)
    in case unknowns
       of []
            -> return ()
          _
            -> throw (ExceptLineMessage n
                      ("Found unknown link keys: " ++ show unknowns))


select :: Int -> String -> [(String, String)] -> Excepted (Maybe String)
select n key kvs
  = case filter (fst .> (== key)) kvs
    of [(_, val)]
         -> return (Just val)
       []
         -> return Nothing
       _
         -> throw (ExceptLineMessage n
                   ("Found repeated link key: " ++ key))




{- *** Parsing Key-Value Pairs *** -}

parseKeyVals :: [Token] -> Excepted [(String, String)]
parseKeyVals
  = dropSpaceToks .> parseKeyVals'

parseKeyVals' :: [Token] -> Excepted [(String, String)]
parseKeyVals' []
  = return []
parseKeyVals' tokens
  = let (before, after) = breakComma tokens
    in do (key, val) <- parseKeyVal before
          kvs <- parseKeyVals after
          return ((key, val) : kvs)


breakComma :: [Token] -> ([Token], [Token])
breakComma tokens
  = let (before, after) = break isComma tokens
    in (before, removePossibleComma after)


isComma :: Token -> Bool
isComma (TnSymbol _ ',')
  = True
isComma _
  = False


removePossibleComma :: [Token] -> [Token]
removePossibleComma (TnSymbol _ ',' : ts)
  = ts
removePossibleComma ts
  = ts


parseKeyVal :: [Token] -> Excepted (String, String)
parseKeyVal
  = dropSpaceToks .> parseKeyVal'


parseKeyVal' :: [Token] -> Excepted (String, String)
parseKeyVal' ((t@(TnWord _ key)) : ts)
  = case dropSpaceToks ts
    of (TnSymbol _ '=' : rest)
         -> do val <- parseVal rest
               return (key, val)
       _
         -> throw (tokenException t "Could not parse key in link.")
parseKeyVal' (t : _)
  = throw (tokenException t "Could not parse key in link.")
parseKeyVal' []
  = throw (ExceptMessage "Could not parse key in link.")


parseVal :: [Token] -> Excepted String
parseVal
  = trimSpaceToks .> parseVal'

parseVal' :: [Token] -> Excepted String
parseVal' [TnWord _ str]
  = return str
parseVal' (t : _)
  = throw (tokenException t "Could not parse value in link.")
parseVal' []
  = throw (ExceptMessage "Could not parse value in link.")
