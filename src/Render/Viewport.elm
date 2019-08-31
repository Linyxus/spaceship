module Render.Viewport exposing (..)

import Objects.Types exposing (Point)

type alias Viewport =
    { center : Point
    , width : Float
    , height : Float
    }
