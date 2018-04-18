module Update
    exposing
        ( update
        , subscriptions
        )

import Msg exposing (..)
import Model exposing (..)
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
                        (Block.updateAt idxs
                            (\e ->
                                case e of
                                    Block.Lit _ ->
                                        case String.toInt str of
                                            Ok x ->
                                                Block.Lit x

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
                                                        |> Block.removeAt dragIdxs
                                                        |> (case Block.exprAt dragIdxs e of
                                                                Just dragExpr ->
                                                                    Block.setAt hoverIdxs dragExpr

                                                                Nothing ->
                                                                    Debug.crash
                                                                        ("Cannot find expr at index: "
                                                                            ++ toString dragIdxs
                                                                        )
                                                           )

                                                LibItem f ->
                                                    e
                                                        |> Block.setAt hoverIdxs
                                                            (mkGetDef model.defs
                                                                f
                                                                (\typ ctnts _ ->
                                                                    Block.defLhsToExpr f
                                                                        (Block.DefLhs typ ctnts)
                                                                )
                                                            )

                                                LibLiteral ->
                                                    e
                                                        |> Block.setAt hoverIdxs (Block.Lit 0)
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
                        |> Maybe.map
                            (Block.reduceCallByValueSelectionAt idxs
                                (mkGetDef model.defs)
                            )
                        |> Maybe.andThen Zipper.fromList
            }
                ! []

        StepTo frames ->
            case model.eval of
                Just _ ->
                    { model | eval = Just frames } ! []

                Nothing ->
                    Debug.crash "Stepping when not showing eval view"
