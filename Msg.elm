module Msg exposing (..)

import Model exposing (..)
import Block
import Mouse


type Msg
    = DragStart Item Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseOver Block.Indices
    | MouseLeave Block.Indices
