
module Block (packBlock, unpackBlock,
              updateHints,
              acrossBlock, acrossBlockIO)
       where

import DataBlock


packBlock :: a -> Block a
packBlock thing
  = (Block (HiNone, HiNone) (False, False) thing)


unpackBlock :: Block a -> a
unpackBlock (Block _ _ thing)
  = thing


updateHints :: (Hint, Hint) -> Block a -> Block a
updateHints hints (Block _ bools thing)
  = Block hints bools thing


acrossBlock :: (a -> b) -> Block a -> Block b
acrossBlock f (Block (hiAbove, hiBelow) (coAbove, coBelow) thing)
  = Block (hiAbove, hiBelow) (coAbove, coBelow) (f thing)


acrossBlockIO :: (a -> IO b) -> Block a -> IO (Block b)
acrossBlockIO f (Block (hiAbove, hiBelow) (coAbove, coBelow) thing)
  = do result <- f thing
       return (Block (hiAbove, hiBelow) (coAbove, coBelow) result)

