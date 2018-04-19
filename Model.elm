module Model exposing (..)

import Block
    exposing
        ( Def(..)
        , DefLhs(..)
        , DefContent(..)
        , Expr(..)
        , Case(..)
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
        -- [hack] bogus value
        (Var "x")


add1 =
    Def
        (DefLhs Block.int
            [ DefVar Block.int "x"
            , DefText "incremented by 1"
            ]
        )
        (App addId
            [ Var "x"
            , Lit 1
            ]
        )


appendId =
    "append"


append =
    let
        aList =
            "a list"

        anotherList =
            "another list"

        listofInt =
            Block.list Block.int
    in
        Def
            (DefLhs listofInt
                [ DefVar listofInt aList
                , DefText "followed by"
                , DefVar listofInt anotherList
                ]
            )
            (CaseStmt (Var aList)
                [ Case Block.emptyListId
                    []
                    (Var anotherList)
                , let
                    firstElement =
                        "first element"

                    restOfTheList =
                        "rest of the list"
                  in
                    Case Block.consId
                        [ firstElement, restOfTheList ]
                        (Block.cons
                            (Var firstElement)
                            (App appendId
                                [ Var restOfTheList
                                , Var anotherList
                                ]
                            )
                        )
                ]
            )


defs =
    Dict.fromList
        [ ( addId, add )
        , ( "add1", add1 )
        , ( appendId, append )
        ]
