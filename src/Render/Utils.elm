module Render.Utils exposing (..)

import Color exposing (Color)
import Canvas
import Canvas.Settings exposing (Setting)
import Objects.Types exposing (Point)
import Render.Painter exposing (..)

changeAlpha : Float -> Color -> Color
changeAlpha alpha c =
    let rgba = Color.toRgba c
    in Color.fromRgba <| { rgba | alpha = alpha }

drawCircle : Point -> Float -> Color -> PainterDraw
drawCircle { x, y } r color =
    PainterDraw [ Canvas.Settings.fill color ] [ PaintShape <| Canvas.circle (x, y) r ]

lighterColor : Color -> Color
lighterColor color =
    let hsla = Color.toHsla color
    in Color.fromHsla { hsla | lightness = (1.0 + hsla.lightness) / 2 }
