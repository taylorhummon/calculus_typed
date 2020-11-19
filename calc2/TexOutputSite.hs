
module TexOutputSite (texOutputSite)
       where

import DataInfo
import DataPage
import DataLocation

import Info (buildInfo)
import Locations (frontLocs, mainLocs, appendLocs, backLocs)
import TexCommon (bs)
import TexFiles (texWriteFile)
import TexEnsureSite (ensureSite)
import InputPage (inputPage)
import UpdateLinks (updateLinks)
import UpdateMath (updateMath)
import Message (exitFail)
import TexOutputPage (outputPage)

import System.IO (Handle, hPutStrLn)


texOutputSite :: Run -> IO ()
texOutputSite run
  = do info <- buildInfo run
       ensureSite
       texWriteFile (action info)
       return ()


action :: Info -> Handle -> IO ()
action info handle
  = do hPutStrLn handle (bs ++ "input{preamble.tex}")
       hPutStrLn handle (bs ++ "input{declarations.tex}")
       hPutStrLn handle (bs ++ "begin{document}")
       -- hPutStrLn handle (bs ++ "frontmatter")
       hPutStrLn handle (bs ++ "input{titlepage.tex}")
       hPutStrLn handle (bs ++ "input{toc.tex}")
       _ <- mapM (processLocation info handle) (frontLocs info)
       -- hPutStrLn handle (bs ++ "mainmatter")
       _ <- mapM (processLocation info handle) (mainLocs info)
       hPutStrLn handle (bs ++ "appendix")       
       _ <- mapM (processLocation info handle) (appendLocs info)
       hPutStrLn handle (bs ++ "backmatter")
       _ <- mapM (processLocation info handle) (backLocs info)
       hPutStrLn handle (bs ++ "end{document}")
       return ()


processLocation :: Info -> Handle -> Location -> IO ()
processLocation info handle loc
  = do mp <- inputPage info loc
       processPage info handle mp


processPage :: Info -> Handle -> Maybe Page-> IO ()
processPage _ _ Nothing
  = exitFail "Exiting now."
processPage info handle (Just page1)
  = do page2 <- updateLinks info page1
       page3 <- updateMath info page2
       _ <- outputPage info handle page3
       return ()

