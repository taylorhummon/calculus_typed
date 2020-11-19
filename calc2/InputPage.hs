
module InputPage (inputPage)
       where

import DataInfo
import DataPage
import DataLocation

import Token (tokenize)
import ParsePage (parsePage)

import Except (Excepted, unwrapExceptedValue)
import Message (messageException)
import Files (readFileByLoc)



inputPage :: Info -> Location -> IO (Maybe Page)
inputPage info loc
  = do excStr <- readFileByLoc loc
       let excPage = createPage info loc excStr
       handlePage loc (unwrapExceptedValue excPage)


createPage :: Info -> Location -> Excepted String -> Excepted Page
createPage info loc excStr
  = do str <- excStr
       tokens <- tokenize str
       parsePage info loc tokens


handlePage :: Location -> Either String Page -> IO (Maybe Page)
handlePage loc (Left str)
  = do messageException loc str
       return Nothing
handlePage _ (Right page)
  = return (Just page)

