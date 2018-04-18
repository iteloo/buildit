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
import List.Zipper as Zipper exposing (Zipper)


type alias Model =
    { dragging : Maybe Drag
    , hover : Maybe Block.Indices
    , defs : Dict.Dict Block.Id Def
    , draftExpr : Maybe Expr
    , eval : Maybe (Zipper EvalFrame)
    }


type Item
    = DraftItem Block.Indices
    | LibItem Block.Id
    | LibLiteral


type alias Drag =
    { itemId : Item
    , confirmed : Bool
    , startPos : Mouse.Position
    , currentPos : Mouse.Position
    }


type alias EvalFrame =
    ( Expr, Block.Indices )


init =
    { dragging = Nothing
    , hover = Nothing
    , defs = defs
    , -- [tmp] hard-coded
      draftExpr = Just Block.testExpr
    , eval = Nothing
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
