
module WebOutputPage (outputPage)
       where

import DataPage
import DataInfo
import DataLocation

import GetImages (getImages)
import WebFiles (ensurePageDir, isPageOutDated, writePage,
                 ensureImageDir, isImageOutDated, writeImage)
import WebBuildPage (buildPage)


outputPage :: Info -> Page -> IO ()
outputPage i (page@(Page loc _ _))
  = do ensurePageDir i loc
       b1 <- outputImages i page
       b2 <- isPageOutDated i loc
       if b1 && b2
         then do _ <- writePage i loc (buildPage i page)
                 return ()
         else do return ()


outputImages :: Info -> Page -> IO Bool
outputImages i (page@(Page loc _ _))
  =  do ensureImageDir i loc
        bools <- mapM (outputImage i loc) (getImages page)
        return (and bools)


outputImage :: Info -> Location -> String -> IO Bool
outputImage i loc image
  = do b <- isImageOutDated i loc image
       if b
         then writeImage i loc image
         else return True
