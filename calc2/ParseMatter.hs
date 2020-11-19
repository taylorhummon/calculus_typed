
module ParseMatter (parseMatters, grabImageUnadorned)
       where

import Compose
import DataMatter
import DataBlock
import DataMath
import DataLocation

import Except (Excepted, Exception(..), throw, reThrow)
import Token (Token(..), dropSpaceToks,
              trimSpaceToks, tokenException)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, grabFirstLoc,
              breakOnGrabLoc)
import ParseBlock (grabBlock, grabBlockNoHints)
import ParseWriting (parseWriting)
import ParseMathLines (parseMathLines)
import Block (packBlock, acrossBlock)


parseMatters :: Location -> [Token] -> Excepted Matters
parseMatters loc
  = dropSpaceToks .> (parseMatters' loc)

parseMatters' :: Location -> [Token] -> Excepted Matters
parseMatters' _ []
   = return []
parseMatters' loc tokens
  = caseGrabbed (grabExplicitMatter loc tokens)
    -- Missed
    (do (matter, rest) <- spanMatter loc tokens
        matters <- parseMatters loc rest
        return (matter : matters)
    )
    -- Grabbed matter rest
    (\ matter rest ->
      do matters <- parseMatters loc rest
         return (matter : matters)
    )


grabExplicitMatter :: Location -> [Token] -> Grabbed (Block Matter)
grabExplicitMatter
  = grabFirstLoc [grabPlain
                 ,grabMathLines
                 ,grabImage
                 ,grabItemize
                 ,grabEnumerate
                 ]


spanMatter :: Location -> [Token] -> Excepted (Block Matter, [Token])
spanMatter loc tokens
  = do (inside, rest) <- breakOnGrabLoc grabExplicitMatter loc tokens
       writing <- parseWriting loc inside
       return (packBlock (MaPlain writing), rest)
                      

{- *** Plain *** -}

grabPlain :: Location -> [Token] -> Grabbed (Block Matter)
grabPlain loc tokens
  = grabBlock loc "matter" tokens parsePlain


parsePlain :: Location -> Int -> [Token] -> Excepted Matter
parsePlain loc _ tokens
  = do writing <- parseWriting loc tokens
       return (MaPlain writing)


{- *** Math *** -}

grabMathLines :: Location -> [Token] -> Grabbed (Block Matter)
grabMathLines loc tokens
  = caseGrabbed (grabMathLinesUnadorned loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed blockMathLines rest
    (\ bml rest ->
       return (Grabbed (acrossBlock MaMathLines bml) rest)
    )


grabMathLinesUnadorned :: Location -> [Token] -> Grabbed (Block MathLines)
grabMathLinesUnadorned loc tokens
  = grabBlock loc "math" tokens parseMathLinesUnadorned


parseMathLinesUnadorned :: Location -> Int -> [Token] -> Excepted MathLines
parseMathLinesUnadorned _ n tokens
  = parseMathLines n tokens


{- *** Image *** -}

grabImage :: Location -> [Token] -> Grabbed (Block Matter)
grabImage loc tokens
  = caseGrabbed (grabImageUnadorned True loc tokens)
    -- Missed
    (return Missed)
    -- Grabbed blockImage rest
    (\ bi rest ->
      return (Grabbed (acrossBlock MaImage bi) rest)
    )
  

grabImageUnadorned :: Bool -> Location -> [Token] -> Grabbed (Block Image)
grabImageUnadorned True loc tokens
  = grabBlock loc "image" tokens parseImageUnadorned
grabImageUnadorned False loc tokens
  = grabBlockNoHints loc "image" tokens parseImageUnadorned


parseImageUnadorned :: Location -> Int -> [Token] -> Excepted Image
parseImageUnadorned loc n tokens
  = do filename <- (parseImageName (trimSpaceToks tokens)
                    `reThrow` registerBadFileName n)
       return (Image loc filename)


registerBadFileName :: Int -> Exception -> Exception
registerBadFileName n ExceptBadFileName
  = ExceptLineMessage n "Could not parse file name."
registerBadFileName _ e
  = e


-- parseImageName
-- throws exception ExceptBadImageName

parseImageName :: [Token] -> Excepted String
parseImageName []
  = return []
parseImageName (TnWord _ str : ts)
  = do rest <- parseImageName ts
       return (str ++ rest)
parseImageName (TnNumber _ str : ts)
  = do rest <- parseImageName ts
       return (str ++ rest)
parseImageName (TnSymbol _ '_' : ts)
  = do rest <- parseImageName ts
       return ('_' : rest)
parseImageName _
  = throw ExceptBadFileName


{- *** List *** -}

grabItemize :: Location -> [Token] -> Grabbed (Block Matter)
grabItemize loc tokens
  = grabBlock loc "itemize" tokens parseItemize
    
    
parseItemize :: Location -> Int -> [Token] -> Excepted Matter
parseItemize loc _ tokens
  = do items <- parseItems loc tokens
       return (MaItemize items)


grabEnumerate :: Location -> [Token] -> Grabbed (Block Matter)
grabEnumerate loc tokens
  = grabBlock loc "enumerate" tokens parseEnumerate
    
    
parseEnumerate :: Location -> Int -> [Token] -> Excepted Matter
parseEnumerate loc _ tokens
  = do items <- parseItems loc tokens
       return (MaEnumerate items)


parseItems :: Location -> [Token] -> Excepted [Block Item]
parseItems loc
  = dropSpaceToks .> (parseItems' loc)

parseItems' :: Location -> [Token] -> Excepted [Block Item]
parseItems' _ []
  = return []
parseItems' loc tokens
  = caseGrabbed (grabItem loc tokens)
    -- Missed
    (throw (tokenException (head tokens) "Expected item."))
    -- Grabbed item rest
    (\ item rest ->
      do items <- parseItems loc rest
         return (item : items)
    )


grabItem :: Location -> [Token] -> Grabbed (Block Item)
grabItem loc tokens
  = grabBlock loc "item" tokens parseItem


parseItem :: Location -> Int -> [Token] -> Excepted Item
parseItem loc _ tokens
  = do matters <- parseMatters loc tokens
       return (Item matters)

