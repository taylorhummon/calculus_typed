
module WebBuildTocTop (buildTocTop)
       where

import DataInfo
import DataLocation

import Info (infoGetPageInfos, formatTitle)
import WebCommon (Html, quote, nl)
import WebBuildTocCommon (buildEntry, subLocation)
import Data.List (find)


buildTocTop :: Info -> Html
buildTocTop info
  = concatMap (partToHtml info) (getParts info)
    ++
    appendicesToHtml info
    ++
    summariesToHtml info


getParts :: Info -> [PageInfo]
getParts i
  = filter isPart (infoGetPageInfos i)

isPart :: PageInfo -> Bool
isPart (PageInfo _ _ (PtPart _))
  = True
isPart _
  = False


partToHtml :: Info -> PageInfo -> Html
partToHtml info pageInfo
  = concat ["<p>", nl,
            "<span class=", quote, "part", quote, ">", nl,
            formatTitle pageInfo, nl,
            "</span>", nl,
            "</p>", nl,
            "<ul>", nl,
            concatMap (chapterToHtml info) (getChapters info loc),
            "</ul>", nl]
  where (PageInfo loc _ _) = pageInfo


getChapters :: Info -> Location -> [PageInfo]
getChapters i loc
  = filter (isChapterIn loc) (infoGetPageInfos i)

isChapterIn :: Location -> PageInfo -> Bool
isChapterIn loc (PageInfo loc' _ (PtChapter _))
  = loc `subLocation` loc'
isChapterIn _ _
  = False


chapterToHtml :: Info -> PageInfo -> Html
chapterToHtml i pageInfo
  = concat ["<li>", nl,
            buildEntry i pageInfo, nl,
            "</li>", nl]


appendicesToHtml :: Info -> Html
appendicesToHtml i
  = appendicesToHtml' i (find isAppendices (infoGetPageInfos i))

appendicesToHtml' :: Info -> Maybe PageInfo -> Html
appendicesToHtml' i (Just pageInfo)
  = concat ["<p>", nl,
            "<span class=", quote, "app", quote, ">", nl,
            buildEntry i pageInfo, nl,
            "</span>", nl,
            "</p>", nl]
appendicesToHtml' _ _
  = []

isAppendices :: PageInfo -> Bool
isAppendices (PageInfo _ _ PtAppendices)
  = True
isAppendices _
  = False


summariesToHtml :: Info -> Html
summariesToHtml i
  = summariesToHtml' i (find isSummaries (infoGetPageInfos i))

summariesToHtml' :: Info -> Maybe PageInfo -> Html
summariesToHtml' i (Just pageInfo)
  = concat ["<p>", nl,
            "<span class=", quote, "summ", quote, ">", nl,
            buildEntry i pageInfo, nl,
            "</span>", nl,
            "</p>", nl]
summariesToHtml' _ _
  = []

isSummaries :: PageInfo -> Bool
isSummaries (PageInfo _ _ PtSummaries)
  = True
isSummaries _
  = False


