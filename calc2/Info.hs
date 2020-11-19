
module Info (infoGetRun
            ,infoGetPageInfos
            ,buildInfo
            ,validLocation
            ,fetchPageInfo
            ,formatTitle
            )
       where

import DataContents
import DataInfo
import DataLocation

import Data.List (find)
import InputContents (inputContents)


-- We'll prefer not to pattern match against an Info.
-- Instead, we'll use "accessor" functions.

infoGetRun :: Info -> Run
infoGetRun (Info run _)
  = run

infoGetPageInfos :: Info -> [PageInfo]
infoGetPageInfos (Info _ pageInfos)
  = pageInfos


-- Build the Info by inputting the contents file

buildInfo :: Run -> IO Info
buildInfo run
  = do contents <- inputContents
       let initialPageInfos = buildInfo' [] contents
           numberedPageInfos = updatePageInfos initialPageInfos
       return (Info run numberedPageInfos)


buildInfo' :: Location -> Contents -> [PageInfo]
buildInfo' loc (Contents (ContentsLine pageId title) subs)
  = pageInfo : concatMap (buildInfo' pageLoc) subs
  where pageInfo = PageInfo pageLoc title PtUnknown
        pageLoc = loc ++ [pageId]


-- Add PageType to each PageInfo.
-- This includes part/chapter/section numbers.

updatePageInfos :: [PageInfo] -> [PageInfo]
updatePageInfos pageInfos
  = updatePageInfos' 0 0 0 pageInfos

updatePageInfos' :: Int -> Int -> Int -> [PageInfo] -> [PageInfo]
updatePageInfos' _ _ _ []
  = []
updatePageInfos' part chapter section (pageInfo : rest)
  = case depth pageInfo
    of 1 -> updatePageInfo PtTop pageInfo
            : updatePageInfos' 0 0 0 rest
       2 -> updateDepthTwo part chapter section (pageInfo : rest)
       3 -> updatePageInfo (PtChapter (chapter + 1)) pageInfo
            : updatePageInfos' part (chapter + 1) 0 rest
       4 -> updatePageInfo (PtSection chapter (section + 1)) pageInfo
            : updatePageInfos' part chapter (section + 1) rest
       _ -> pageInfo : updatePageInfos' part chapter section rest


depth :: PageInfo -> Int
depth (PageInfo loc _ _)
  = length loc


updateDepthTwo :: Int -> Int -> Int -> [PageInfo] -> [PageInfo]
updateDepthTwo part chapter section (pageInfo : rest)
  | isAppendices pageInfo
    = updatePageInfo PtAppendices pageInfo
      : updateAppendices part chapter section rest
  | isSummaries pageInfo
    = updatePageInfo PtSummaries pageInfo
      : updateSummaries part chapter section rest
  | otherwise
    = updatePageInfo (PtPart (part + 1)) pageInfo
      : updatePageInfos' (part + 1) chapter section rest
updateDepthTwo _ _ _ []
  = []


updatePageInfo :: PageType -> PageInfo -> PageInfo
updatePageInfo pt (PageInfo loc phrase _)
  = PageInfo loc phrase pt


isAppendices :: PageInfo -> Bool
isAppendices (PageInfo [_, "appendices"] _ _)
  = True
isAppendices _
  = False


isSummaries :: PageInfo -> Bool
isSummaries (PageInfo [_, "summaries"] _ _)
  = True
isSummaries _
  = False


updateAppendices :: Int -> Int -> Int -> [PageInfo] -> [PageInfo]
updateAppendices part chapter section pageInfos
  = updateAppendix 1 apps
    ++
    updatePageInfos' part chapter section rest
  where (apps, rest) = span isAppendix pageInfos


isAppendix :: PageInfo -> Bool
isAppendix (PageInfo [_, "appendices", _] _ _)
  = True
isAppendix _
  = False


updateAppendix :: Int -> [PageInfo] -> [PageInfo]
updateAppendix _ []
  = []
updateAppendix i (pageInfo : rest)
  = updatePageInfo (PtAppendix i) pageInfo
    : updateAppendix (i + 1) rest


updateSummaries :: Int -> Int -> Int -> [PageInfo] -> [PageInfo]
updateSummaries part chapter section pageInfos
  = updateSummary summs
    ++
    updatePageInfos' part chapter section rest
  where (summs, rest) = span isSummary pageInfos


isSummary :: PageInfo -> Bool
isSummary (PageInfo [_, "summaries", _] _ _)
  = True
isSummary _
  = False


updateSummary :: [PageInfo] -> [PageInfo]
updateSummary []
  = []
updateSummary (PageInfo loc str _ : rest)
  = PageInfo loc str PtSummary : updateSummary rest



-- Other useful functions for operating on the Info

validLocation :: Info -> Location -> Bool
validLocation i loc
  = case find (hasLoc loc) (infoGetPageInfos i)
    of Just _
         -> True
       Nothing
         -> False


fetchPageInfo :: Info -> Location -> PageInfo
fetchPageInfo i loc
  = case find (hasLoc loc) (infoGetPageInfos i)
    of Just pageInfo
         -> pageInfo
       Nothing
         -> error "Could not produce page info."


hasLoc :: Location -> PageInfo -> Bool
hasLoc loc (PageInfo loc' _ _)
  = loc == loc'


formatTitle :: PageInfo -> String
formatTitle (PageInfo _ title pt)
  = formatPageNumber pt ++ title


formatPageNumber :: PageType -> String
formatPageNumber PtTop
  = ""
formatPageNumber (PtPart i)
  = "Part " ++ numerals i ++ ". "
formatPageNumber (PtChapter i)
  = "Chapter " ++ stringFromInt i ++ ". "
formatPageNumber (PtSection i j)
  = stringFromInt i ++ "." ++ stringFromInt j ++ " "
formatPageNumber PtAppendices
  = ""
formatPageNumber (PtAppendix i)
  = "Appendix " ++ letter i ++ ". "
formatPageNumber PtSummaries
  = ""
formatPageNumber PtSummary
  = ""
formatPageNumber PtUnknown
  = ""


stringFromInt :: Int -> String
stringFromInt
  = show


letter :: Int -> String
letter i
  = [['A' .. 'Z'] !! (i - 1)]


numerals :: Int -> String
numerals i
  = case i
    of 1 -> "I"
       2 -> "II"
       3 -> "III"
       4 -> "IV"
       5 -> "V"
       6 -> "VI"
       7 -> "VII"
       8 -> "VIII"
       9 -> "IX"
       10 -> "X"
       _ -> error "I can only count so high."
