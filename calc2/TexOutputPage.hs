
module TexOutputPage (outputPage)
       where

import DataInfo
import DataPage
import DataLocation

import TexFiles (writePage, ensureImageDir,
                 isImageOutDated, writeImage)
import TexBuildPage (buildPage)
import GetImages (getImages)

import System.IO (Handle)


outputPage :: Info -> Handle -> Page -> IO ()
outputPage i handle (page@(Page loc _ _))
  = do _ <- outputImages page
       writePage loc handle (buildPage i page)
       return ()


outputImages :: Page -> IO ()
outputImages (page@(Page loc _ _))
  =  do ensureImageDir loc
        _ <- mapM (outputImage loc) (getImages page)
        return ()


outputImage :: Location -> String -> IO ()
outputImage loc image
  = do b <- isImageOutDated loc image
       if b
         then do _ <- writeImage loc image
                 return ()
         else do return ()
