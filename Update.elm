module Update
    exposing
        ( update
        , subscriptions
        )

import Msg exposing (..)
import Model exposing (..)
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
            { model | dragging = Nothing } ! []

        MouseOver id ->
            model ! Debug.log id []

        MouseLeave id ->
            model ! Debug.log id []
