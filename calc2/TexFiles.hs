
module TexFiles (ensureTexDir, ensurePreamble,
                 ensureDeclarations, ensureTitlePage, ensureToc,
                 texWriteFile, writePage,
                 ensureImageDir, isImageOutDated,
                 writeImage, pathFromLoc)
       where

import DataLocation

import qualified Files as F (Path, ImageName(..), topDir, ensureDir,
                             ensureFileCopy, isImageOutDated, writeImage)
import System.IO (IOMode(..), Handle, withFile, hPutStr)



texDir :: F.Path
texDir
  = F.topDir ++ "tex/"


texFile :: F.Path
texFile
  = "calculustyped.tex"


resourcesDir :: F.Path
resourcesDir
  = F.topDir ++ "resources/"


resourcesTexDir :: F.Path
resourcesTexDir
  = resourcesDir ++ "tex/"


preambleFile :: F.Path  
preambleFile
  = "preamble.tex"


declarationsFile :: F.Path  
declarationsFile
  = "declarations.tex"


titlePageFile :: F.Path  
titlePageFile
  = "titlepage.tex"


tocFile :: F.Path  
tocFile
  = "toc.tex"


ensureTexDir :: IO ()
ensureTexDir
  = F.ensureDir texDir


texWriteFile :: (Handle -> IO ()) -> IO ()
texWriteFile h
  = withFile (texDir ++ texFile) WriteMode h


ensurePreamble :: IO ()
ensurePreamble
  = F.ensureFileCopy
    "preamble file"
    (resourcesTexDir ++ preambleFile)
    (texDir ++ preambleFile)


ensureDeclarations :: IO ()
ensureDeclarations
  = F.ensureFileCopy
    "declarations file"
    (resourcesTexDir ++ declarationsFile)
    (texDir ++ declarationsFile)


ensureTitlePage :: IO ()
ensureTitlePage
  = F.ensureFileCopy
    "title page file"
    (resourcesTexDir ++ titlePageFile)
    (texDir ++ titlePageFile)


ensureToc :: IO ()
ensureToc
  = F.ensureFileCopy
    "table of contents file"
    (resourcesTexDir ++ tocFile)
    (texDir ++ tocFile)


writePage :: Location -> Handle -> String -> IO ()
writePage _ handle str
  = hPutStr handle str


pathImgDirOut :: Location -> F.Path
pathImgDirOut loc
  = texDir ++ pathFromLoc loc ++ "/"


pathImgFileOut :: Location -> F.ImageName -> F.Path
pathImgFileOut loc (F.ImageName _ file extension)
  = pathImgDirOut loc ++ file ++ "." ++ extension


pathFromLoc :: Location -> F.Path
pathFromLoc (pageId : [])
  = pageId
pathFromLoc (pageId : rest)
  = pageId ++ "/" ++ pathFromLoc rest
pathFromLoc []
  = []


ensureImageDir :: Location -> IO ()
ensureImageDir loc
  = F.ensureDir (pathImgDirOut loc)


isImageOutDated :: Location -> F.Path -> IO Bool
isImageOutDated loc file
  = F.isImageOutDated
    loc
    (pathImgFileOut loc (makeImageName file))
    (makeImageName file)


writeImage :: Location -> F.Path -> IO Bool
writeImage loc file
  = F.writeImage loc
    (pathImgFileOut loc (makeImageName file))
    (makeImageName file)


makeImageName :: String -> F.ImageName
makeImageName str
  = F.ImageName "ink" str "pdf"

