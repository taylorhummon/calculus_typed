
module DataContents
       where


data Contents =
  Contents ContentsLine [Contents]

data ContentsLine =
  ContentsLine String String

