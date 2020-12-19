module Bex.Lang exposing (BExpr(..), BexModule, Definition)

import List.Nonempty exposing (Nonempty)


type alias BexModule =
    { name : String
    , exposing_ : Nonempty String
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
