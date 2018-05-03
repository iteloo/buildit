module Model exposing (..)

import Expr
    exposing
        ( Expr(..)
        , Case(..)
        , Name
        )
import Block
    exposing
        ( Def(..)
        , Block(..)
        , BlkContent(..)
        )
import BlockExample
import Helper
import Dict
import Mouse
import List.Zipper as Zipper exposing (Zipper)


type alias Model =
    { dragging : Maybe Drag
    , hover : Maybe Expr.Indices
    , blockData : BlockData
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


type alias BlockData =
    Dict.Dict Name Block


init =
    { dragging = Nothing
    , hover = Nothing
    , blockData = BlockExample.blockData
    , defs = BlockExample.defs
    , -- [tmp] hard-coded
      draftExpr = Just BlockExample.range56
    , eval = Nothing
    }


getBlock : BlockData -> Name -> Block
getBlock bdata name =
    case Dict.get name bdata of
        Just block ->
            block

        Nothing ->
            Debug.crash <| Helper.unwords [ "Block data missing for", name ]



-- HELPER


mkGetDef : Dict.Dict Expr.Name Def -> Block.GetDef a
mkGetDef defs id handler =
    case Dict.get id defs of
        Just (Def params rhs) ->
            handler params rhs

        Nothing ->
            Debug.crash ("No matching definition found for " ++ id)
