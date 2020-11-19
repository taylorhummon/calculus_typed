
module DataMatter
       where

import DataBlock
import DataWriting
import DataMath
import DataLocation


type Matters = [Block Matter]

data Matter =
  MaPlain Writing |
  MaMathLines MathLines |
  MaImage Image |
  MaItemize [Block Item] |
  MaEnumerate [Block Item]
  
data Image =
  Image Location String

data Item =
  Item Matters

