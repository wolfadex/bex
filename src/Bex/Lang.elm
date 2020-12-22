module Bex.Lang exposing
    ( BExpr(..)
    , BexModule
    , BexModulePartial
    , Definition
    )

import List.Nonempty exposing (Nonempty)


type alias BexModulePartial =
    { name : String
    , exposing_ : Nonempty String
    , imports : List String
    , definitions : String
    }


type alias BexModule =
    { name : String
    , exposing_ : Nonempty String
    , imports : List String
    , definitions : Nonempty Definition
    }


type alias Definition =
    { name : String
    , body : Nonempty BExpr
    }


type BExpr
    = BInt Int
    | BString String
    | BFunc String
    | BOper String
    | BQuote BExpr
