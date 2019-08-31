module Objects.Wave exposing (..)

import Canvas exposing (..)
import Objects.Types exposing (Point)
import Color exposing (Color)
import Render.Painter exposing (Painter, rectRange)
import Render.Utils exposing (changeAlpha, drawCircle)

type alias Wave =
    { center : Point
    , minR : Float
    , maxR : Float
    , currentR : Float
    , color : Color
    , duration : Float
    }

mod : Float -> Float -> Float
mod x y = x - (toFloat <| floor (x / y)) * y

updateWave : Float -> Wave -> Wave
updateWave t wave = { wave
                    | currentR = wave.minR + mod (wave.currentR + t / wave.duration * (wave.maxR - wave.minR) - wave.minR) (wave.maxR - wave.minR)
                    }

paintWave : Wave -> List Painter
paintWave { center, minR, maxR, currentR, color, duration } =
    let stage = (currentR - minR) / (maxR - currentR)
        newColor = changeAlpha (1.0 - stage) color
        draw = drawCircle { x = 0, y = 0 } currentR newColor
    in List.singleton
        { range = rectRange center (maxR*2) (maxR*2)
        , center = center
        , draw = draw
        }
