
import DataInfo

import System.Environment (getProgName)
import Message (exitFail)
import SystemCalls (bulldozeWeb, bulldozePublish)
import WebOutputSite (webOutputSite)
import TexOutputSite (texOutputSite)



main :: IO ()
main
  = do run <- getRun
       outputSite run


getRun :: IO Run
getRun
  = do pn <- getProgName
       getRun' pn


getRun' :: String -> IO Run
getRun' "calc2web"
  = return RunWeb
getRun' "calc2web-rebuild"
  = return RunWebRebuild
getRun' "calc2web-publish"
  = return RunWebPublish
getRun' "calc2tex"
  = return RunTex
getRun' _
  = exitFail (concat ["Please run as:", nl,
                      "  calc2web", nl,
                      "  calc2web-rebuild", nl,
                      "  calc2web-publish", nl,
                      "  calc2tex"])

nl :: String
nl = "\n"


outputSite :: Run -> IO ()
outputSite RunWeb
  = do webOutputSite RunWeb
outputSite RunWebRebuild
  = do _ <- bulldozeWeb
       webOutputSite RunWebRebuild
outputSite RunWebPublish
  = do _ <- bulldozePublish
       webOutputSite RunWebPublish
outputSite RunTex
  = do texOutputSite RunTex

