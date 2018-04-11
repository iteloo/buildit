module Main exposing (main)

import Model
import Update
import View
import Html


main =
    Html.program
        { init = ( Model.init, Cmd.none )
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
