module Main exposing (main)

import Model
import Msg
import Update
import View
import Html


main : Program Never Model.Model Msg.Msg
main =
    Html.program
        { init = ( Model.init, Cmd.none )
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
