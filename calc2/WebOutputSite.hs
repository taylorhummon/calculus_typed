
module WebOutputSite (webOutputSite)
       where

import DataInfo
import DataLocation

import WebFiles (ensureSite)
import Info (buildInfo)
import WebLocations (getLocsToProcess)
import InputPage (inputPage)
import UpdateLinks (updateLinks)
import UpdateMath (updateMath)
import UpdateCollapse (updateCollapse)
import WebOutputPage (outputPage)


webOutputSite :: Run -> IO ()
webOutputSite run
  = do info <- buildInfo run
       _ <- ensureSite info
       locs <- getLocsToProcess info
       _ <- mapM (processLocation info) locs
       return ()


processLocation :: Info -> Location -> IO ()
processLocation info loc
  = do mp <- inputPage info loc
       case mp
         of Just page1
              -> do page2 <- updateLinks info page1
                    page3 <- updateMath info page2
                    page4 <- updateCollapse info page3
                    _ <- outputPage info page4
                    return ()
            Nothing
              -> do return ()

