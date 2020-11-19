
module MathTransform (transformCommand
                     ,transformCommandOpt
                     )
       where

import DataMath


transformCommand :: String -> [Math] -> Math

transformCommand "Point" [math]
  = [MeCommand "Point" [], MeSubScript math]

transformCommand "Metric" [math]
  = [MeCommand "Metric" [],
     MeSubScript math]

transformCommand "PolyMetric" [math, math']
  = [MeCommand "PolyMetric" [],
     MeSubScript math,
     MeSuperScript math']

transformCommand "Vector" [math]
  = [MeCommand "Vector" [],
     MeSubScript math]

transformCommand "PolyVector" [math, math']
  = [MeCommand "PolyVector" [],
     MeSubScript math,
     MeSuperScript math']

transformCommand "Ruler" [math]
  = [MeCommand "Ruler" [],
     MeSubScript math]

transformCommand "PolyRuler" [math, math']
  = [MeCommand "PolyRuler" [],
     MeSubScript math,
     MeSuperScript math']

transformCommand "Blip" [math]
  = [MeCommand "Blip" [],
     MeSubScript math]

transformCommand "Interval" [math]
  = [MeCommand "Interval" [],
     MeSubScript math]

transformCommand "Box" [math, math']
  = [MeCommand "BoxType" [],
     MeSubScript math,
     MeSuperScript math']

transformCommand "Path" [math]
  = [MeCommand "Path" [],
     MeSubScript math]

transformCommand "slope" [math]
  = [MeCommand "operatorname"
     [[MeCommand "overset"
       [[MeCommand "mathit" [math]],
        [MeCommand "slope" []]]]]]

transformCommand "bend" [math]
  = [MeCommand "operatorname"
     [[MeCommand "overset"
       [[MeCommand "mathit" [math]],
        [MeCommand "bend" []]]]]]

transformCommand "overwarp" [math, math']
  = [MeCommand "operatorname"
     [[MeCommand "overset"
       [[MeCommand "mathit" [math ++ math']],
        [MeCommand "warp" []]]]]]

transformCommand "directional" [math]
  = [MeCommand "operatorname"
     [[MeCommand "overset"
       [[MeCommand "mathit" [math]],
        [MeCommand "directional" []]]]]]

transformCommand "natural" []
  = [MeCommand "naturalbase" []]

transformCommand "sum" []
  = [MeCommand "sumcomponent" []]

transformCommand "product" []
  = [MeCommand "productcomponent" []]

transformCommand "deriv" [math]
  = [MeCommand "overset" [math, [MeCommand "diff" []]]]

transformCommand "partial" [math]
  = [MeCommand "overset" [math, [MeCommand "diff" []]]]

transformCommand "metdiff" []
  = [MeCommand "hat" [[MeCommand "diff" []]]]

transformCommand "ruldiff" []
  = [MeCommand "bar" [[MeCommand "diff" []]]]
  -- = [MeCommand "overline" [[MeCommand "diff" []]]]

transformCommand "isa" []
  = [MeSymbol ':']

transformCommand "fun" [math]
  = [MeCommand "mathrel" [[MeCommand "overset" [math, [MeCommand "leftarrow" []]]]]]

transformCommand "met" [math]
  = [MeCommand "hat" [[MeCommand "standard" []]], MeSubScript math]

transformCommand "vec" [math]
  = [MeCommand "vec" [[MeCommand "standard" []]], MeSubScript math]

transformCommand "rul" [math]
  = [MeCommand "bar" [[MeCommand "standard" []]], MeSubScript math]
  -- = [MeCommand "overline" [[MeCommand "standard" []]], MeSubScript math]

transformCommand "times" []
  = [MeCommand "mult" []]

transformCommand "change" [math]
  = [MeCommand "change" [], MeSubScript math]

transformCommand "and" []
  = [MeCommand "andSpaced" []]

transformCommand "or" []
  = [MeCommand "orSpaced" []]

transformCommand "r" []
  = [MeCommand "rad" []]

transformCommand "a" []
  = [MeCommand "ang" []]

transformCommand "matrix" [math]
  = [MeCommand "begin" [mathString "bmatrix"]]
    ++ math ++
    [MeCommand "end" [mathString "bmatrix"]]

transformCommand "path" [math]
  = [MeCommand "left\\{" []]
    ++
    [MeCommand "begin" [mathString "aligned"]]
    ++ math ++
    [MeCommand "end" [mathString "aligned"]]
    ++
    [MeCommand "right." []]

transformCommand "align" []
  = [MeSymbol '&']

transformCommand "newline" []
  = [MeSymbol '\\', MeSymbol '\\']

transformCommand str maths
  = [MeCommand str maths]






transformCommandOpt :: String -> Maybe Math -> [Math] -> Math

transformCommandOpt "root" maybeMath [math']
  = transformRoot maybeMath math'

transformCommandOpt "parens" maybeMath [math']
  = transformParens maybeMath math'

transformCommandOpt "set" maybeMath [math']
  = transformSet maybeMath math'

transformCommandOpt "apply" maybeMath [math1, math2]
  = transformApply maybeMath math1 math2

transformCommandOpt "measure" maybeMath [math1, math2]
  = transformMeasure maybeMath math1 math2

transformCommandOpt "blip" maybeMath [math']
  = transformBlip maybeMath math'

transformCommandOpt "interval" maybeMath [math1, math2]
  = transformInterval maybeMath math1 math2

transformCommandOpt "integral" maybeMath [math1, math2]
  = transformIntegral maybeMath math1 math2

transformCommandOpt "int" maybeMath [math1, math2]
  = transformInt maybeMath math1 math2

transformCommandOpt str maybeMath maths
  = [MeCommandOpt str maybeMath maths]




transformRoot :: Maybe Math -> Math -> Math
transformRoot Nothing math'
  = [MeCommandOpt "sqrt" (Just [MeNumber "2"]) [math']]
transformRoot (Just math) math'
  = [MeCommandOpt "sqrt" (Just math) [math']]


transformParens :: Maybe Math -> Math -> Math
transformParens (Just [MeNumber "1"]) math'
  = [MeCommand "bigl" [], MeSymbol '(']
    ++ math' ++
    [MeCommand "bigr" [], MeSymbol ')']
transformParens (Just [MeNumber "2"]) math'
  = [MeCommand "Bigl" [], MeSymbol '(']
    ++ math' ++
    [MeCommand "Bigr" [], MeSymbol ')']
transformParens (Just [MeNumber "3"]) math'
  = [MeCommand "biggl" [], MeSymbol '(']
    ++ math' ++
    [MeCommand "biggr" [], MeSymbol ')']
transformParens _ math'
  = [MeSymbol '(']
    ++ math' ++
    [MeSymbol ')']


transformSet :: Maybe Math -> Math -> Math
transformSet (Just [MeNumber "1"]) math'
  = [MeCommand "bigl" [], MeCommand "{" []]
    ++ enclose math' ++
    [MeCommand "bigr" [], MeCommand "}" []]
transformSet (Just [MeNumber "2"]) math'
  = [MeCommand "Bigl" [], MeCommand "{" []]
    ++ enclose math' ++
    [MeCommand "Bigr" [], MeCommand "}" []]
transformSet (Just [MeNumber "3"]) math'
  = [MeCommand "biggl" [], MeCommand "{" []]
    ++ enclose math' ++
    [MeCommand "biggr" [], MeCommand "}" []]
transformSet _ math'
  = [MeCommand "{" []] ++ enclose math' ++ [MeCommand "}" []]


transformApply :: Maybe Math -> Math -> Math -> Math
transformApply (Just [MeNumber "1"]) math1 math2
  = [MeCommand "bigl" [], MeSymbol '[']
    ++ math1
    ++ [MeSymbol ';', MeSpace]
    ++ math2
    ++ [MeCommand "bigr" [], MeSymbol ']']
transformApply (Just [MeNumber "2"]) math1 math2
  = [MeCommand "of" []]
    ++ [MeCommand "Bigl" [], MeSymbol '[']
    ++ math1
    ++ [MeSymbol ';', MeSpace]
    ++ math2
    ++ [MeCommand "Bigr" [], MeSymbol ']']
transformApply (Just [MeNumber "3"]) math1 math2
  = [MeCommand "biggl" [], MeSymbol '[']
    ++ math1
    ++ [MeSymbol ';', MeSpace]
    ++ math2
    ++ [MeCommand "biggr" [], MeSymbol ']']
transformApply _ math1 math2
  = [MeSymbol '[']
    ++ math1
    ++ [MeSymbol ';', MeSpace]
    ++ math2
    ++ [MeSymbol ']']


transformMeasure :: Maybe Math -> Math -> Math -> Math
transformMeasure (Just [MeNumber "1"]) math1 math2
  = [MeCommand "bigl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ [MeCommand "mathbin" [[MeCommand "big" [], MeSymbol '|']]]
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "bigr" [], MeCommand "rangle" []]
transformMeasure (Just [MeNumber "2"]) math1 math2
  = [MeCommand "Bigl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ [MeCommand "mathbin" [[MeCommand "Big" [], MeSymbol '|']]]
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "Bigr" [], MeCommand "rangle" []]
transformMeasure (Just [MeNumber "3"]) math1 math2
  = [MeCommand "biggl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ [MeCommand "mathbin" [[MeCommand "bigg" [], MeSymbol '|']]]
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "biggr" [], MeCommand "rangle" []]
transformMeasure _ math1 math2
  = [MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ [MeCommand "mathbin" [[MeSymbol '|']]]
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "rangle" []]


transformBlip :: Maybe Math -> Math -> Math
transformBlip Nothing math'
  = [MeSymbol '['] ++ math' ++ [MeSymbol ']']
transformBlip (Just math) math'
  = [MeSymbol '['] ++ math' ++ [MeSymbol ']'] ++ [MeSubScript math]


transformInterval :: Maybe Math -> Math -> Math -> Math
transformInterval Nothing math1 math2
  = [MeSymbol '[']
    ++ math1 ++
    [MeSymbol ',', MeSpace]
    ++ math2 ++
    [MeSymbol ']']
transformInterval (Just math) math1 math2
  = [MeSymbol '[']
    ++ math1 ++
    [MeSymbol ',', MeSpace]
    ++ math2 ++
    [MeSymbol ']']
    ++ [MeSubScript math]


transformIntegral :: Maybe Math -> Math -> Math -> Math
transformIntegral (Just math) math1 math2
  = [MeCommand "biggl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ integral
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "biggr" [], MeCommand "rangle" []]
  where integral
          = [MeCommand "mathrel"
             [[MeCommand "overset"
               [math,
                [MeCommand "displaystyle" [[MeCommand "int" []]]]]]]]

transformIntegral Nothing math1 math2
  = [MeCommand "biggl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ integral
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "biggr" [], MeCommand "rangle" []]
  where integral
          = [MeCommand "mathrel"
             [[MeCommand "displaystyle" [[MeCommand "int" []]]]]]


transformInt :: Maybe Math -> Math -> Math -> Math
transformInt (Just math) math1 math2
  = [MeCommand "Bigl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ integral
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "Bigr" [], MeCommand "rangle" []]
  where integral
          = [MeCommand "mathrel"
             [[MeCommand "overset"
               [math,
                [MeCommand "textstyle" [[MeCommand "int" []]]]]]]]

transformInt Nothing math1 math2
  = [MeCommand "Bigl" [], MeCommand "langle" []]
    ++ [MeSymbol '{'] ++ math1 ++ [MeSymbol '}']
    ++ integral
    ++ [MeSymbol '{'] ++ math2 ++ [MeSymbol '}']
    ++ [MeCommand "Bigr" [], MeCommand "rangle" []]
  where integral
          = [MeCommand "mathrel"
             [[MeCommand "textstyle" [[MeCommand "int" []]]]]]








mathString :: String -> Math
mathString
  = map MeVariable

enclose :: Math -> Math
enclose math
  = [MeSymbol ' ', MeSymbol '{']
    ++ math
    ++ [MeSymbol '}', MeSymbol ' ']


