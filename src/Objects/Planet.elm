module Objects.Planet exposing (..)

import Objects.Types exposing (Point)
import Canvas
import Objects.Wave exposing (..)
import Color exposing (Color)
import Render.Utils exposing (lighterColor, drawCircle)
import Render.Painter exposing (..)

type alias Planet =
    { mass : Float
    , wave : Wave
    , center : Point
    , r : Float
    , color : Color
    }

updatePlanet : Float -> Planet -> Planet
updatePlanet t pl = { pl | wave = updateWave t pl.wave }

mkPlanet : Point -> Float -> Float -> Color -> Planet
mkPlanet center r m color =
    let wave = { center = center
               , minR = r
               , maxR = 2.5 * r
               , currentR = r
               , color = lighterColor color
               , duration = 1500
               }
    in { mass = m
       , wave = wave
       , center = center
       , r = r
       , color = color
       }

paintPlanet : Planet -> List Painter
paintPlanet { wave, center, r, color } =
    let renderPlanet = drawCircle { x = 0, y = 0 } r color
    in
        paintWave wave ++
        [ { range = rectRange center (r*2) (r*2)
          , center = center
          , draw = renderPlanet
          }
        ]
