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


update msg model =
    case msg of
        DragStart id pos ->
            { model
                | dragging =
                    Just
                        { itemId = id
                        , startPos = pos
                        , currentPos = pos
                        }
            }
                ! []

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
                        (case Maybe.map2 ((,)) model.dragging model.hover of
                            Nothing ->
                                identity

                            Just ( { itemId }, idxs ) ->
                                mkGetDef model.defs
                                    itemId
                                    (\typ ctnts _ ->
                                        Block.updateAt idxs
                                            (Block.defLhsToExpr itemId
                                                (Block.DefLhs typ ctnts)
                                            )
                                    )
                        )
                        model.draftExpr
            }
                ! []

        MouseOver idxs ->
            { model | hover = Just idxs } ! Debug.log (toString idxs) []

        MouseLeave idxs ->
            { model | hover = Nothing } ! Debug.log ("l" ++ toString idxs) []
