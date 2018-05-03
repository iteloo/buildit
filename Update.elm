module Update
    exposing
        ( update
        , subscriptions
        )

import Msg exposing (..)
import Model exposing (..)
import Expr
import Block
import Mouse
import List.Zipper as Zipper exposing (Zipper)


subscriptions model =
    case model.dragging of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.moves DragAt
                , Mouse.ups DragEnd
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LitEdit idxs str ->
            { model
                | draftExpr =
                    Maybe.map
                        (Expr.updateAt idxs
                            (\e ->
                                case e of
                                    Expr.Lit _ ->
                                        case String.toInt str of
                                            Ok x ->
                                                Expr.Lit x

                                            Err _ ->
                                                e

                                    _ ->
                                        Debug.crash "Not a literal"
                            )
                        )
                        model.draftExpr
            }
                ! []

        DragStart id pos ->
            { model
                | dragging =
                    Just
                        { itemId = Debug.log "drag-start: " id
                        , confirmed = False
                        , startPos = pos
                        , currentPos = pos
                        }
            }
                ! []

        -- [hack] needed to intercept parents mousedown event
        HoleMouseDown ->
            model ! []

        DragAt pos ->
            let
                dist pos1 pos2 =
                    (abs (pos1.x - pos2.x)) ^ 2 + (abs (pos1.y - pos2.y)) ^ 2

                -- [q] in pixels?
                threshold =
                    10
            in
                { model
                    | dragging =
                        Maybe.map
                            (\d ->
                                { d
                                    | confirmed =
                                        d.confirmed
                                            || dist d.currentPos d.startPos
                                            > threshold
                                    , currentPos = pos
                                }
                            )
                            model.dragging
                }
                    ! []

        DragEnd pos ->
            let
                updExpr =
                    case model.dragging |> Maybe.map .confirmed of
                        Just True ->
                            Maybe.map
                                (\e ->
                                    case Maybe.map2 ((,)) model.dragging model.hover of
                                        Nothing ->
                                            e

                                        Just ( { itemId }, hoverIdxs ) ->
                                            case itemId of
                                                DraftItem dragIdxs ->
                                                    e
                                                        -- [note] must remove first!
                                                        |> Expr.removeAt dragIdxs
                                                        |> (case Expr.exprAt dragIdxs e of
                                                                Just dragExpr ->
                                                                    Expr.setAt hoverIdxs dragExpr

                                                                Nothing ->
                                                                    Debug.crash
                                                                        ("Cannot find expr at index: "
                                                                            ++ toString dragIdxs
                                                                        )
                                                           )

                                                LibItem f ->
                                                    e
                                                        |> Expr.setAt hoverIdxs
                                                            (Block.toExpr f
                                                                (getBlock model.blockData f)
                                                            )

                                                LibLiteral ->
                                                    e
                                                        |> Expr.setAt hoverIdxs (Expr.Lit 0)
                                )

                        _ ->
                            identity
            in
                { model
                    | dragging = Nothing
                    , draftExpr = updExpr model.draftExpr
                }
                    ! []

        MouseOver idxs ->
            { model | hover = Just idxs }
                ! Debug.log ("hover: " ++ toString idxs) []

        MouseLeave idxs ->
            { model | hover = Nothing } ! []

        Reduce idxs ->
            { model
                | eval =
                    model.draftExpr
                        |> Maybe.andThen
                            (Block.evalCBVAt idxs
                                (mkGetDef model.defs)
                                >> Zipper.fromList
                            )
                        |> Maybe.map Zipper.last
            }
                ! []

        StepTo frames ->
            case model.eval of
                Just _ ->
                    { model | eval = Just frames } ! []

                Nothing ->
                    Debug.crash "Stepping when not showing eval view"
