
module DataBlock
       where


data Block a =
  Block (Hint, Hint) (Bool, Bool) a


data Hint =
  HiNone |
  HiSpace String |
  HiPageBreak
  deriving Eq


