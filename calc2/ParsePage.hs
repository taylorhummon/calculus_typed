
module ParsePage (parsePage)
       where

import Compose
import DataInfo
import DataPage
import DataLocation

import Except (Excepted)
import Token (Token, dropSpaceToks)
import Parse (Grabbed, Grabbed'(..),
              caseGrabbed, breakOnGrabLoc, grabCommand)
import ParseGroup (parseGroups)
import ParsePhrase (parsePhrase)
import Info (fetchPageInfo)


parsePage :: Info -> Location -> [Token] -> Excepted Page
parsePage info loc tokens
  = do subpages <- parseSubPages loc tokens
       let title = getTitle info loc
       return (Page loc title subpages)


getTitle :: Info -> Location -> String
getTitle info loc
  = let (PageInfo _ title _) = fetchPageInfo info loc
    in title


parseSubPages :: Location -> [Token] -> Excepted [SubPage]
parseSubPages loc
  = dropSpaceToks .> (parseSubPages' loc)

parseSubPages' :: Location -> [Token] -> Excepted [SubPage]
parseSubPages' _ []
  = return []
parseSubPages' loc tokens
  = caseGrabbed (grabSubPage loc tokens)
    -- Missed
    (do (before, after) <- breakOnGrabLoc grabSubPage loc tokens
        subpage <- parseSubPage loc before
        subpages <- parseSubPages loc after
        return (subpage : subpages)
    )
    -- Grabbed subpage rest
    (\ subpage rest ->
      do subpages <- parseSubPages loc rest
         return (subpage : subpages)
    )


grabSubPage :: Location -> [Token] -> Grabbed SubPage
grabSubPage loc tokens
  = caseGrabbed (grabCommand "subsection" 1 tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, args) rest
    (\ (_, args) rest ->
      do let [inside] = args
         title <- parsePhrase inside
         (inside', rest') <- breakOnGrabLoc grabSubPage loc rest
         groups <- parseGroups loc inside'
         let subpage = SubPage (Just title) groups
         return (Grabbed subpage rest')
    )


parseSubPage :: Location -> [Token] -> Excepted SubPage
parseSubPage loc tokens
  = do groups <- parseGroups loc tokens
       return (SubPage Nothing groups)
    

