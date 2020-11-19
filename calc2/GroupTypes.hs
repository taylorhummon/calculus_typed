
module GroupTypes (groupTypes, fancyGroupTypes)
       where

import Compose
import DataGroup


groupTypes :: [(GroupType, String)]
groupTypes
  = concat[[(GtPlain, "group")],
           fancyGroupTypes $> map (\ (a, (b, _)) -> (a, b))]


fancyGroupTypes :: [(GroupType, (String, String))]
fancyGroupTypes
  = [(GtDefinition, ("definition", "Definition."))
    ,(GtFormula, ("formula", "Formula."))
    ,(GtFormulas, ("formulas", "Formulas."))
    ,(GtImportant, ("important", "Important."))
    ,(GtLaw, ("law", "Law."))
    ,(GtLaws, ("laws", "Laws."))
    ,(GtQuestion, ("question", "Question."))
    ,(GtTheorem, ("theorem", "Theorem."))
    ,(GtExample, ("example", "Example."))
    ,(GtWhy, ("why", "Why?"))
    ]

