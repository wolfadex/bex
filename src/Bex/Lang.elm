module Bex.Lang exposing
    ( BExpr(..)
    , BexModule
    , BexModulePartial
    , Definition
    , builtinWords
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
    | BFunc String
    | BOper String
    | BQuote BExpr


builtinWords : List String
builtinWords =
    [ "drop"
    , "swap"
    , "dup"
    , "rotate"
    , "over"
    , "apply"
    , "then"
    , "emit"
    , "identity"
    ]
