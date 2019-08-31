module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Task
import Time
import Objects.Wave exposing (..)
import Render.Painter exposing (..)
import Objects.Planet exposing (..)

type alias Model =
    { width : Float
    , height : Float
    , planet : List Planet }

type Msg
    = Frame Float
    | GetViewport Viewport

init : () -> (Model, Cmd Msg)
init _ =
    ( { width = 0, height = 0
      , planet = [ mkPlanet { x = -100, y = 100 } 50 100 Color.blue
                 , mkPlanet { x = 100, y = 100 } 50 100 Color.green
                 , mkPlanet { x = -100, y = -100 } 50 100 Color.yellow
                 , mkPlanet { x = 100, y = -100 } 50 100 Color.red
                 ]}
    , Task.perform GetViewport getViewport
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame delta ->
                        ({ model | planet = List.map (updatePlanet delta) <| model.planet }, Cmd.none)

                    GetViewport data ->
                        ( { model
                            | width = data.viewport.width
                            , height = data.viewport.height
                          }
                        , Cmd.none
                        )
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


centerX width =
    width / 2


centerY height =
    height / 2


view : Model -> Html Msg
view { width, height, planet } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round width, round height )
            []
            (
             [ clearScreen width height
             -- , shapes [ transform [ rotate (degrees 50) ]] [ rect (40, 40) 20 20 ]
             ] ++ runPainters { center = { x = 0, y = 0 }, width = width, height = height}
                  (List.map paintPlanet >> List.concat <| planet)
            )
        ]


clearScreen width height =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render count width height =
    let
        size =
            min width height / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        rotation =
            degrees (count * 3)

        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
        shapes
        [ transform
            [ translate (centerX width) (centerY height)
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]
