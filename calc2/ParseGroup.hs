
module ParseGroup (parseGroups)
       where

import Compose
import DataGroup
import DataBlock
import DataLocation

import Except (Excepted)
import Token (Token(..), dropSpaceToks)
import Parse (Grabbed, caseGrabbed,
              breakOnGrabLoc, grabFirst)
import ParseBlock (grabBlock)
import GroupTypes (groupTypes)
import ParseLayout (parseLayouts)
import Block (packBlock)


parseGroups :: Location -> [Token] -> Excepted Groups
parseGroups loc
  = dropSpaceToks .> (parseGroups' loc)

parseGroups' :: Location -> [Token] -> Excepted Groups
parseGroups' _ []
  = return []
parseGroups' loc tokens
  = do (group, rest) <- spanGroup loc tokens
       groups <- parseGroups loc rest
       return (group : groups)


spanGroup :: Location -> [Token] -> Excepted (Block Group, [Token])
spanGroup loc tokens
  = caseGrabbed (grabExplicitGroup loc tokens)
    -- Missed
    (do (inside, rest) <- breakOnGrabLoc grabExplicitGroup loc tokens
        group <- parseImplicitGroup loc inside
        return (group, rest)
    )
    -- Grabbed group rest
    (\ group rest ->
      return (group, rest)
    )


parseImplicitGroup :: Location -> [Token] -> Excepted (Block Group)
parseImplicitGroup loc tokens
  = do layouts <- parseLayouts loc tokens
       return (packBlock (Group GtPlain layouts))


grabExplicitGroup :: Location -> [Token] -> Grabbed (Block Group)
grabExplicitGroup loc
  = groupTypes
    $> map (grabGroup loc)
    $> grabFirst


grabGroup :: Location -> (GroupType, String) -> [Token] -> Grabbed (Block Group)
grabGroup loc (gt, name) tokens
  = grabBlock loc name tokens (parseGroup gt)


parseGroup :: GroupType -> Location -> Int -> [Token] -> Excepted Group
parseGroup gt loc _ tokens
  = do layouts <- parseLayouts loc tokens
       return (Group gt layouts)

