
module DataInfo
       where

import DataLocation


data Info =
  Info Run [PageInfo]

data Run =
    RunWeb |
    RunWebRebuild |
    RunWebPublish |
    RunTex

data PageInfo =
  PageInfo Location String PageType

data PageType =
  PtTop |
  PtPart Int |
  PtChapter Int |
  PtSection Int Int |
  PtAppendices |
  PtAppendix Int |
  PtSummaries |
  PtSummary |
  PtUnknown

