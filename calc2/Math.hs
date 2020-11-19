
module Math (commands, commandOpts)
       where


-- See MathTransform.hs for the transformations promised by this list.
-- See Symbols.hs for a list of symbol characters allowed in math.

-- BI means Built In to TeX/LaTeX/AmsLaTeX
-- JC means defined in Jax Config

commands :: [(String, Int)]
commands
  = [
    ("exp"              , 0),    -- BI
    ("log"              , 0),    -- BI
    ("cos"              , 0),    -- BI
    ("sin"              , 0),    -- BI
    ("approx"           , 0),    -- BI
    ("neq"              , 0),    -- BI
    ("le"               , 0),    -- BI
    ("ge"               , 0),    -- BI
    ("prime"            , 0),    -- BI
    ("ldots"            , 0),    -- BI
    ("cdots"            , 0),    -- BI
    ("frac"             , 2),    -- BI

    ("Number"           , 0),    -- JC
    ("Point"            , 1),    -- \Point_{}
    ("Metric"           , 1),    -- \Metric_{}
    ("PolyMetric"       , 2),    -- \PolyMetric_{}^{}
    ("Vector"           , 1),    -- \Vector_{}
    ("PolyVector"       , 2),    -- \PolyVector_{}^{}
    ("Ruler"            , 1),    -- \Ruler_{}
    ("PolyRuler"        , 2),    -- \PolyRuler_{}^{}
    ("Blip"             , 1),    -- \Blip_{}
    ("Interval"         , 1),    -- \Interval_{}
    ("Box"              , 2),    -- \BoxType_{}^{}
    ("Path"             , 1),    -- \Path_{}

    -- \operatorname{\overset{\mathit{x}}{\slope}} z
    
    ("slope"            , 1),    -- overset{}{\slope}
    ("bend"             , 1),    -- \overset{}{\bend}
    ("warp"             , 0),    -- JC
    ("overwarp"         , 2),    -- \overset{{}{}}{\warp}
    ("tangent"          , 0),    -- JC
    ("hessian"          , 0),    -- JC
    ("discriminant"     , 0),    -- JC
    ("directional"      , 1),    -- overset{}{\directional}
    ("gradient"         , 0),    -- JC
    ("magnitude"        , 0),    -- JC
    ("transpose"        , 0),    -- JC
    ("position"         , 0),    -- JC
    ("velocity"         , 0),    -- JC
    ("speed"            , 0),    -- JC
    ("natural"          , 0),    -- naturalbase
    ("revolution"       , 0),    -- JC
    ("sum"              , 0),    -- sumcomponent
    ("product"          , 0),    -- productcomponent
    
    ("diff"             , 0),    -- JC
    ("deriv"            , 1),    -- \overset{}{\diff}
    ("partial"          , 1),    -- \overset{}{\diff}
    ("metdiff"          , 0),    -- \hat{\diff}
    ("ruldiff"          , 0),    -- \bar{\diff}
    ("boundary"         , 0),    -- JC
    
    ("isa"              , 0),    -- :
    ("met"              , 1),    -- \hat{\standard}_{}
    ("vec"              , 1),    -- \vec{\standard}_{}
    ("rul"              , 1),    -- \bar{\standard}_{}
    ("fun"              , 1),    -- \overset{}{\leftarrow}
    ("depends"          , 0),    -- JC
    ("of"               , 0),    -- JC
    ("times"            , 0),    -- \mult
    ("change"           , 1),    -- \change_{}
    ("degrees"          , 0),    -- JC
    ("sliced"           , 0),    -- JC
    ("and"              , 0),    -- \andSpaced
    ("or"               , 0),    -- \orSpaced
    ("r"                , 0),    -- \rad
    ("a"                , 0),    -- \ang

    ("matrix"           , 1),    -- \begin{bmatrix} {} \end{bmatrix}
    ("path"             , 1),    -- \left\{ \begin{aligned} {} \end{aligned} \right.
    ("align"            , 0),    -- &
    ("newline"          , 0)     -- \\

    ]


commandOpts :: [(String, Int)]
commandOpts
  = [
    ("root"             , 1),    -- \sqrt{}                       opt arg is inset
    ("parens"           , 1),    -- ({})                          opt arg is size
    ("set"              , 1),    -- \{ {} \}                      opt arg is size
    ("apply"            , 2),    -- [ {}; {} ]                    opt arg is size
    ("measure"          , 2),    -- \langle {} | {} \rangle       opt arg is size
    ("blip"             , 1),    -- [ {} ]                        opt arg is subscript
    ("interval"         , 2),    -- [ {}, {} ]                    opt arg is subscript
    ("integral"         , 2),    -- \langle {} \int {} \rangle    opt arg is overset
    ("int"              , 2)     -- \langle {} \int {} \rangle    opt arg is overset
    ]


-- See ParseMath.hs for a few special commands supported by the parser for
-- math mode.

--   "unsafe" is a special command that allows unverified math to be passed
--   through the parser untouched.

--   "text" is a special command for inserting text into math.


