
module TexCommon (Tex, bs, quote, nl, pnl)
       where


type Tex = String


bs :: Tex
bs = "\\"

quote :: Tex
quote = "\""

nl :: Tex
nl = "\n"

pnl :: Tex
pnl = "%\n"
