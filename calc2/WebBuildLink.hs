
module WebBuildLink (buildLink
                    ,buildPrevLink
                    ,buildNextLink
                    ,buildUpLink
                    )
       where

import DataPage
import DataInfo
import DataLink
import DataLocation
import DataPhrase

import Locations (allLocsExceptParts)
import Info (infoGetPageInfos, fetchPageInfo)
import WebCommon (Html, quote)
import WebBuildPhrase (buildPhrase)
import WebUrls (Url, urlLink, urlLanding)
import Data.List (find)


buildLink :: Info -> Link -> Html
buildLink _ (LkExternal str phrase)
  = concat ["<a href=", quote, str, quote, ">",
            buildPhrase phrase,
            "</a>"]
buildLink _ (LkTarget _)
  = error "Encountered LkTarget at build time."
buildLink info (LkInternal loc phrase)
  = concat ["<a href=", quote, urlLink info loc, quote, ">",
            buildPhrase phrase,
            "</a>"]
buildLink info (LkString loc str)
  = concat ["<a href=", quote, urlLink info loc, quote, ">",
            str,
            "</a>"]
buildLink _ (LkFail phrase)
  = buildPhrase phrase


buildPrevLink :: Info -> Page -> Html
buildPrevLink info (Page loc _ _)
  = case prevLoc info loc
    of Nothing
         -> "Previous"
       Just loc'
         -> buildLink info (LkString loc' "Previous")


prevLoc :: Info -> Location -> Maybe Location
prevLoc info loc
  = prevLoc' loc (allLocsExceptParts info)

prevLoc' :: Location -> [Location] -> Maybe Location
prevLoc' _ [_]
  = Nothing
prevLoc' loc (p : (rest @ (loc' : _)))
  | loc == loc' = Just p
  | otherwise   = prevLoc' loc rest
prevLoc' _ []
  = Nothing


buildNextLink :: Info -> Page -> Html
buildNextLink info (Page loc _ _)
  = case nextLoc info loc
    of Nothing
         -> "Next"
       Just loc'
         -> buildLink info (LkString loc' "Next")


nextLoc :: Info -> Location -> Maybe Location
nextLoc info loc
  = nextLoc' loc (allLocsExceptParts info)

nextLoc' :: Location -> [Location] -> Maybe Location
nextLoc' _ [_]
  = Nothing
nextLoc' loc (loc' : (rest @ (n : _)))
  | loc == loc' = Just n
  | otherwise   = nextLoc' loc rest
nextLoc' _ []
  = Nothing


buildUpLink :: Info -> Page -> Html
buildUpLink info (Page loc _ _)
  = case upLoc info loc
    of Nothing
         -> buildUpLink' info (urlLanding info)
       Just loc'
         -> buildLink info (LkString loc' "Up")

buildUpLink' :: Info -> Maybe Url -> Html
buildUpLink' _ Nothing
  = "Up"
buildUpLink' info (Just landing)
  = buildLink info (LkExternal landing [PeWord "Up"])


upLoc :: Info -> Location -> Maybe Location
upLoc info loc
  = upLoc' info (fetchPageInfo info loc)


upLoc' :: Info -> PageInfo -> Maybe Location
upLoc' _ (PageInfo _ _ PtTop)
  = Nothing
upLoc' info (PageInfo _ _ (PtPart _))
  = Just (topLocation info)
upLoc' info (PageInfo _ _ (PtChapter _))
  = Just (topLocation info)
upLoc' info (PageInfo loc _ (PtSection _ _))
  = chapterLocation info loc
upLoc' info (PageInfo _ _ PtSummaries)
  = Just (topLocation info)
upLoc' info (PageInfo _ _ PtSummary)
  = summariesLocation info
upLoc' info (PageInfo _ _ PtAppendices)
  = Just (topLocation info)
upLoc' info (PageInfo _ _ (PtAppendix _))
  = appendicesLocation info
upLoc' _ (PageInfo _ _ PtUnknown)
  = Nothing


chapterLocation :: Info -> Location -> Maybe Location
chapterLocation _ []
  = Nothing
chapterLocation _ loc
  = Just (init loc)


summariesLocation :: Info -> Maybe Location
summariesLocation i
  = case find isSummaries (infoGetPageInfos i)
    of Nothing
         -> Nothing
       Just (PageInfo loc _ _)
         -> Just loc

isSummaries :: PageInfo -> Bool
isSummaries (PageInfo _ _ PtSummaries)
  = True
isSummaries _
  = False


appendicesLocation :: Info -> Maybe Location
appendicesLocation i
  = case find isAppendices (infoGetPageInfos i)
    of Nothing
         -> Nothing
       Just (PageInfo loc _ _)
         -> Just loc

isAppendices :: PageInfo -> Bool
isAppendices (PageInfo _ _ PtAppendices)
  = True
isAppendices _
  = False


topLocation :: Info -> Location
topLocation i
  = topLocation' (infoGetPageInfos i)

topLocation' :: [PageInfo] -> Location
topLocation' (PageInfo [topId] _ _ : _)
  = [topId]
topLocation' (_ : rest)
  = topLocation' rest
topLocation' []
  = error "Top page is missing!"

