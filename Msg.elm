module Msg exposing (..)

import Model exposing (..)
import Expr
import Mouse
import List.Zipper as Zipper exposing (Zipper)


type Msg
    = LitEdit Expr.Indices String
    | DragStart Item Mouse.Position
    | HoleMouseDown
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | MouseOver Expr.Indices
    | MouseLeave Expr.Indices
    | Reduce Expr.Indices
    | StepTo (Zipper EvalFrame)
