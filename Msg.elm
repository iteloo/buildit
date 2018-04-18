module Msg exposing (..)

import Model exposing (..)
import Block
import Mouse
import List.Zipper as Zipper exposing (Zipper)


type Msg
    = LitEdit Block.Indices String
    | DragStart Item Mouse.Position
    | HoleMouseDown
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseOver Block.Indices
    | MouseLeave Block.Indices
    | Reduce Block.Indices
    | StepTo (Zipper EvalFrame)
