module Update
    exposing
        ( update
        , subscriptions
        )

import Msg exposing (..)
import Model exposing (..)
import Block
import Mouse


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
        DragStart id pos ->
            { model
                | dragging =
                    Just
                        { itemId = Debug.log "drag-start: " id
                        , startPos = pos
                        , currentPos = pos
                        }
            }
                ! []

        -- [hack] needed to intercept parents mousedown event
        HoleMouseDown ->
            model ! []

        DragAt pos ->
            { model
                | dragging =
                    Maybe.map
                        (\d ->
                            { d | currentPos = pos }
                        )
                        model.dragging
            }
                ! []

        DragEnd pos ->
            { model
                | dragging = Nothing
                , draftExpr =
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
                        )
                        model.draftExpr
            }
                ! []

        MouseOver idxs ->
            { model | hover = Just idxs }
                ! Debug.log ("hover: " ++ toString idxs) []

        MouseLeave idxs ->
            { model | hover = Nothing } ! []

        Reduce idxs ->
            { model
                | draftExpr =
                    Maybe.map
                        (Block.updateAt idxs
                            (Block.reduceCallByValue (mkGetDef model.defs))
                        )
                        model.draftExpr
            }
                ! []
