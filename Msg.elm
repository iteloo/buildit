module Msg exposing (..)

import Block
import Mouse


type Msg
    = DragStart Block.Id Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseOver Block.Indices
    | MouseLeave Block.Indices
