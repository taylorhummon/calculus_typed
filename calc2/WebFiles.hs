
module WebFiles
       (ensureSite,
        ensurePageDir, isPageOutDated, writePage,
        ensureImageDir, isImageOutDated, writeImage)
       where
       

import DataInfo
import DataLocation

import Info (infoGetRun)
import WebTemplate (ensureTemplate)
import qualified Files as F (Path, ImageName(..), topDir, ensureDir,
                             ensureFileCopy, ensureLink,
                             isPageOutDated, writePage,
                             isImageOutDated, writeImage)
import Data.List (intercalate)


webDir :: Info -> F.Path
webDir i
  = case infoGetRun i
    of RunWebPublish
         -> "publish/"
       _
         -> "web/"


resourcesDir :: F.Path
resourcesDir
  = "resources/"


resourcesWebDir :: F.Path
resourcesWebDir
  = resourcesDir ++ "web/"


styles :: [(String, F.Path)]
styles
  = [("calculus stylesheet",    "ct.css")
    ,("normalizing stylesheet", "normalize.css")]


jaxDirIn :: F.Path
jaxDirIn
  = "/usr/local/share/javascript/mathjax"

jaxDir :: F.Path
jaxDir
  = "mathjax"


jaxConfigFile :: F.Path
jaxConfigFile
  = "jaxconfig.js"


missingFile :: F.Path
missingFile
  = "missing.html"


htaccessFileIn :: F.Path
htaccessFileIn
  = "htaccess"


htaccessFileOut :: F.Path
htaccessFileOut
  = ".htaccess"


favIconsDir :: F.Path
favIconsDir
  = resourcesWebDir ++ "icons/"

favIcons :: [F.Path]
favIcons
  = ["android-chrome-192x192.png",
     "favicon-16x16.png",
     "manifest.json",
     "mstile-310x310.png",
     "android-chrome-512x512.png",
     "favicon-32x32.png",
     "mstile-144x144.png",
     "mstile-70x70.png",
     "apple-touch-icon.png",
     "favicon.gif",
     "mstile-150x150.png",
     "safari-pinned-tab.svg",
     "browserconfig.xml",
     "favicon.ico",
     "mstile-310x150.png"]


pathPgDirOut :: Info -> Location -> F.Path
pathPgDirOut i loc
  = webDir i ++ pathFromLoc loc


pathPgFileOut :: Info -> Location -> F.Path
pathPgFileOut i loc
  = pathPgDirOut i loc ++ "index.html"


pathImgDirOut :: Info -> Location -> F.Path
pathImgDirOut 
  = pathPgDirOut


pathImgFileOut :: Info -> Location -> F.ImageName -> F.Path
pathImgFileOut i loc (F.ImageName _ file extension)
  = pathImgDirOut i loc ++ file ++ "." ++ extension


-- discard top pageId (calculus) when creating path
pathFromLoc :: Location -> F.Path
pathFromLoc []
  = []
pathFromLoc (_ : [])
  = []
pathFromLoc (_ : rest)
  = intercalate "/" rest ++ "/"



{- *** Ensuring Output *** -}

ensureSite :: Info -> IO ()
ensureSite i
  = do ensureWebDir i
       ensureStyles i
       ensureJax i
       ensureMissing i
       ensureHtaccess i
       ensureFavIcons i


ensureWebDir :: Info -> IO ()
ensureWebDir i
  = F.ensureDir (F.topDir ++ webDir i)


ensureStyles :: Info -> IO ()
ensureStyles i
  = do _ <- mapM (ensureStyleSheet i) styles
       return ()


ensureStyleSheet :: Info -> (String, F.Path) -> IO ()
ensureStyleSheet i (str, filename)
  = F.ensureFileCopy str
    (F.topDir ++ resourcesWebDir ++ filename)
    (F.topDir ++ webDir i ++ filename)


ensureJax :: Info -> IO ()
ensureJax i
  = do ensureJaxDir i
       ensureJaxConfig i


ensureJaxDir :: Info -> IO ()
ensureJaxDir i
  = case (infoGetRun i)
    of RunWebPublish
         -> return ()    -- jax is already on the server
       _
         -> F.ensureLink
            "mathjax symlink"
            jaxDirIn
            (F.topDir ++ webDir i ++ jaxDir)


ensureJaxConfig :: Info -> IO ()
ensureJaxConfig i
  = ensureTemplate i
    "mathjax config"
    (F.topDir ++ resourcesWebDir ++ jaxConfigFile)
    (F.topDir ++ webDir i ++ jaxConfigFile)


ensureMissing :: Info -> IO ()
ensureMissing i
  = ensureTemplate i
    "404 page"
    (F.topDir ++ resourcesWebDir ++ missingFile)
    (F.topDir ++ webDir i ++ missingFile)


ensureHtaccess :: Info -> IO ()
ensureHtaccess i
  = ensureTemplate i
    "htaccess"
    (F.topDir ++ resourcesWebDir ++ htaccessFileIn)
    (F.topDir ++ webDir i ++ htaccessFileOut)


ensureFavIcons :: Info -> IO ()
ensureFavIcons i
  = do _ <- mapM (ensureFavIcon i) favIcons
       return ()

ensureFavIcon :: Info -> F.Path -> IO ()
ensureFavIcon i filename
  = do F.ensureFileCopy
         filename
         (F.topDir ++ favIconsDir ++ filename)
         (F.topDir ++ webDir i ++ filename)



{- *** Outputting a Page *** -}

ensurePageDir :: Info -> Location -> IO ()
ensurePageDir i loc
  = F.ensureDir (F.topDir ++ pathPgDirOut i loc)


isPageOutDated :: Info -> Location -> IO Bool
isPageOutDated i loc
  = F.isPageOutDated loc (F.topDir ++ pathPgFileOut i loc)


writePage :: Info -> Location -> String -> IO Bool
writePage i loc out
  = F.writePage loc (F.topDir ++ pathPgFileOut i loc) out



{- *** Outputting an Image *** -}

ensureImageDir :: Info -> Location -> IO ()
ensureImageDir i loc
  = F.ensureDir (F.topDir ++ pathImgDirOut i loc)


isImageOutDated :: Info -> Location -> String -> IO Bool
isImageOutDated i loc file
  = F.isImageOutDated
    loc
    (F.topDir ++ pathImgFileOut i loc (makeImageName file))
    (makeImageName file)


writeImage :: Info -> Location -> String -> IO Bool
writeImage i loc file
  = F.writeImage
    loc
    (F.topDir ++ pathImgFileOut i loc (makeImageName file))
    (makeImageName file)


makeImageName :: String -> F.ImageName
makeImageName str
  = F.ImageName "web" str "svg"

