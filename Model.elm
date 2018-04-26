module Model exposing (..)

import Expr
    exposing
        ( Expr(..)
        , Case(..)
        )
import Block
    exposing
        ( Def(..)
        , Block(..)
        , BlkContent(..)
        )
import BlockExample
import Dict
import Mouse
import List.Zipper as Zipper exposing (Zipper)


type alias Model =
    { dragging : Maybe Drag
    , hover : Maybe Expr.Indices
    , defs : Dict.Dict Expr.Name Def
    , draftExpr : Maybe Expr
    , eval : Maybe (Zipper EvalFrame)
    }


type Item
    = DraftItem Expr.Indices
    | LibItem Expr.Name
    | LibLiteral


type alias Drag =
    { itemId : Item
    , confirmed : Bool
    , startPos : Mouse.Position
    , currentPos : Mouse.Position
    }


type alias EvalFrame =
    ( Expr, Expr.Indices )


init =
    { dragging = Nothing
    , hover = Nothing
    , defs = BlockExample.defs
    , -- [tmp] hard-coded
      draftExpr = Just BlockExample.range56
    , eval = Nothing
    }



-- HELPER


mkGetDef : Dict.Dict Expr.Name Def -> Block.GetDef a
mkGetDef defs id handler =
    case Dict.get id defs of
        Just (Def (Block typ ctnts) rhs) ->
            handler typ ctnts rhs

        Nothing ->
            Debug.crash ("No matching definition found for " ++ id)
