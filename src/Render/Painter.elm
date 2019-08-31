module Render.Painter exposing (..)

import Canvas exposing (Renderable)
import Objects.Types exposing (Point)
import Render.Viewport exposing (Viewport)
import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced exposing (transform, translate)

type alias Range = Viewport -> Bool

type PaintAction
    = PaintShape Canvas.Shape
    | PaintText Point String
    -- | PaintTexture Point Canvas.Texture

type alias Painter =
    { range : Range
    , center : Point
    -- , renderable : List Setting -> List Renderable
    , draw : PainterDraw
    }

type alias PainterDraw =
    { settings : List Setting
    , actions : List PaintAction
    }

rectRange : Point -> Float -> Float -> Range
rectRange center w h v =
    let dx = abs <| v.center.x - center.x
        dy = abs <| v.center.y - center.y
    in
        (dx < (w+v.width) / 2) && (dy < (h+v.height) / 2)

getTransforms : Viewport -> Point -> List Setting
getTransforms v { x, y } =
    let dx = x - v.center.x
        dy = y - v.center.y
        realX = v.width/2 + dx
        realY = v.height/2 + dy
    in [ transform [ translate realX realY ]]

renderAction : List Setting -> PaintAction -> Renderable
renderAction st act =
    case act of
        PaintShape shape -> Canvas.shapes st [ shape ]
        PaintText pos str -> Canvas.text st (pos.x, pos.y) str

renderDraw : PainterDraw -> List Renderable
renderDraw { settings, actions } = actions |> List.map (renderAction settings)

wrapDraw : List Setting -> PainterDraw -> PainterDraw
wrapDraw st painter = { painter | settings = st ++ painter.settings }

runPainter : Viewport -> Painter -> List Renderable
runPainter v painter =
    if painter.range v then
        painter.draw |> wrapDraw (getTransforms v painter.center) >> renderDraw
    else []

runPainters : Viewport -> List Painter -> List Renderable
runPainters v = List.map (runPainter v) >> List.concat
