module Model exposing (..)

import Block
    exposing
        ( Def(..)
        , DefLhs(..)
        , DefContent(..)
        , Expr(..)
        )
import Dict
import Mouse


type alias Model =
    { dragging : Maybe Drag
    , defs : Dict.Dict Block.Id Def
    }


type alias Drag =
    { itemId : Block.Id
    , startPos : Mouse.Position
    , currentPos : Mouse.Position
    }


init =
    { dragging = Nothing
    , defs = defs
    }



-- HELPER


mkGetDef : Dict.Dict Block.Id Def -> Block.GetDef a
mkGetDef defs id handler =
    case Dict.get id defs of
        Just (Def (DefLhs typ ctnts) rhs) ->
            handler typ ctnts rhs

        Nothing ->
            Debug.crash ("No matching definition found for " ++ id)



-- [tmp] for testing


addId =
    "add"


add =
    Def
        (DefLhs Block.int
            [ DefVar Block.int "x"
            , DefText "+"
            , DefVar Block.int "y"
            ]
        )
        (Var "x")


defs =
    Dict.fromList
        [ ( addId, add )
        ]
