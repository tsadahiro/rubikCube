module Cube2x2 exposing (..)

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Html.Events
import Length
import Pixels
import Point3d
import Sphere3d
import Scene3d
import Scene3d.Material as Material
import LineSegment3d
import Viewpoint3d
import Browser
import Time
import Quantity
import Axis3d
import SymGrp exposing (..)

type alias Panel = {id : Int
                   ,color : String
                   ,position : Int
                   }

type alias Config = List Panel

type State = Waiting
           | Moving
    
type alias Model = {conf : Config
                   ,state : State
                   }

type Msg =  Elapsed Time.Posix
    | Front
    | Left
    | Top

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

init : () -> (Model, Cmd Msg)
init _ =
    (Model initPanels Waiting, Cmd.none)

initPanel : Int -> Panel
initPanel i =
    let
        color = case (i-1)//4 of
                    0 -> "red"
                    1 -> "blue"
                    2 -> "green"
                    3 -> "yellow"
                    4 -> "orange"
                    5 -> "white"
                    _ -> "black"
    in
        Panel i color i

initPanels : List Panel
initPanels =
    List.map initPanel (List.range 1 24)

rotF : List Panel -> List Panel
rotF panels =
    let
        sigma = prod (cycle [1,2,3,4] 24)  <|
                prod (cycle [7,12,17,14] 24) (cycle [6,11,20,13] 24)
    in
        List.map (\p -> {p | position = act sigma p.position}) panels

rotL : List Panel -> List Panel
rotL panels =
    let
        sigma = prod (cycle [5,6,7,8] 24)  <|
                prod (cycle [1,13,23,9] 24) (cycle [4,16,22,12] 24)
    in
        List.map (\p -> {p | position = act sigma p.position}) panels

rotT : List Panel -> List Panel
rotT panels =
    let
        sigma = prod (cycle [1,5,21,17] 24)  <|
                prod (cycle [2,6,22,18] 24) (cycle [9,10,11,12] 24)
    in
        List.map (\p -> {p | position = act sigma p.position}) panels


        
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Front -> ({model| conf = rotF model.conf}, Cmd.none)
        Left -> ({model| conf = rotL model.conf}, Cmd.none)
        Top -> ({model| conf = rotT model.conf}, Cmd.none)
        _ -> (model, Cmd.none)

panelView: Panel -> Scene3d.Entity coordinates
panelView panel =
    let
        material = case panel.color of
                       "red" ->  Material.nonmetal {baseColor = Color.red
                                                   ,roughness = 0.2
                                                   }
                       "blue" -> Material.nonmetal {baseColor = Color.blue
                                                   ,roughness = 0.2
                                                   }
                       "green" -> Material.nonmetal {baseColor = Color.green
                                                    ,roughness = 0.2
                                                    }
                       "yellow" -> Material.nonmetal {baseColor = Color.yellow
                                                     ,roughness = 0.2
                                                     }
                       "orange" ->  Material.nonmetal {baseColor = Color.lightOrange
                                                      ,roughness = 0.2
                                                      }
                       "white" ->  Material.nonmetal {baseColor = Color.white
                                                     ,roughness = 0.2
                                                     }
                       _ ->  Material.nonmetal {baseColor = Color.black
                                               ,roughness = 0.2
                                               }
    in
        case panel.position of
            1 -> Scene3d.quad material
                 (Point3d.meters 0 1 0)
                 (Point3d.meters 0 1 1)
                 (Point3d.meters 1 1 1)
                 (Point3d.meters 1 1 0)
            2 -> Scene3d.quad material
                 (Point3d.meters 0 1 0)
                 (Point3d.meters -1 1 0)
                 (Point3d.meters -1 1 1)
                 (Point3d.meters 0 1 1)
            3 -> Scene3d.quad material
                 (Point3d.meters 0 1 0)
                 (Point3d.meters 0 1 -1)
                 (Point3d.meters -1 1 -1)
                 (Point3d.meters -1 1 0)
            4 -> Scene3d.quad material
                 (Point3d.meters 0 1 0)
                 (Point3d.meters 1 1 0)
                 (Point3d.meters 1 1 -1)
                 (Point3d.meters 0 1 -1)
            5 -> Scene3d.quad material (Point3d.meters 1 0 0)
                 (Point3d.meters 1 0 1)
                 (Point3d.meters 1 -1 1)
                 (Point3d.meters 1 -1 0)
            6 -> Scene3d.quad material
                 (Point3d.meters 1 0 0)
                 (Point3d.meters 1 1 0)
                 (Point3d.meters 1 1 1)
                 (Point3d.meters 1 0 1)
            7 -> Scene3d.quad material
                 (Point3d.meters 1 0 0)
                 (Point3d.meters 1 1 0)
                 (Point3d.meters 1 1 -1)
                 (Point3d.meters 1 0 -1)
            8 -> Scene3d.quad material
                 (Point3d.meters 1 0 0)
                 (Point3d.meters 1 -1 0)
                 (Point3d.meters 1 -1 -1)
                 (Point3d.meters 1 0 -1)
            9 -> Scene3d.quad material
                 (Point3d.meters 0 0 1)
                 (Point3d.meters 0 -1 1)
                 (Point3d.meters 1 -1 1)
                 (Point3d.meters 1 0 1)
            10 -> Scene3d.quad material
                  (Point3d.meters 0 0 1)
                  (Point3d.meters -1 0 1)
                  (Point3d.meters -1 -1 1)
                  (Point3d.meters 0 -1 1)
            11 -> Scene3d.quad material
                  (Point3d.meters 0 0 1)
                  (Point3d.meters 0 1 1)
                  (Point3d.meters -1 1 1)
                  (Point3d.meters -1 0 1)
            12 -> Scene3d.quad material
                  (Point3d.meters 0 0 1)
                  (Point3d.meters 1 0 1)
                  (Point3d.meters 1 1 1)
                  (Point3d.meters 0 1 1)
            13 -> Scene3d.quad material
                 (Point3d.meters 0 0 -1)
                 (Point3d.meters 0 -1 -1)
                 (Point3d.meters 1 -1 -1)
                 (Point3d.meters 1 0 -1)
            14 -> Scene3d.quad material
                  (Point3d.meters 0 0 -1)
                  (Point3d.meters -1 0 -1)
                  (Point3d.meters -1 -1 -1)
                  (Point3d.meters 0 -1 -1)
            15 -> Scene3d.quad material
                  (Point3d.meters 0 0 -1)
                  (Point3d.meters 0 1 -1)
                  (Point3d.meters -1 1 -1)
                  (Point3d.meters -1 0 -1)
            16 -> Scene3d.quad material
                  (Point3d.meters 0 0 -1)
                  (Point3d.meters 1 0 -1)
                  (Point3d.meters 1 1 -1)
                  (Point3d.meters 0 1 -1)
            17 -> Scene3d.quad material
                  (Point3d.meters -1 0 0)
                  (Point3d.meters -1 0 1)
                  (Point3d.meters -1 -1 1)
                  (Point3d.meters -1 -1 0)
            18-> Scene3d.quad material
                 (Point3d.meters -1 0 0)
                 (Point3d.meters -1 1 0)
                 (Point3d.meters -1 1 1)
                 (Point3d.meters -1 0 1)
            19 -> Scene3d.quad material
                 (Point3d.meters -1 0 0)
                 (Point3d.meters -1 1 0)
                 (Point3d.meters -1 1 -1)
                 (Point3d.meters -1 0 -1)
            20 -> Scene3d.quad material
                 (Point3d.meters -1 0 0)
                 (Point3d.meters -1 -1 0)
                 (Point3d.meters -1 -1 -1)
                 (Point3d.meters -1 0 -1)
            21 -> Scene3d.quad material
                 (Point3d.meters 0 -1 0)
                 (Point3d.meters 0 -1 1)
                 (Point3d.meters 1 -1 1)
                 (Point3d.meters 1 -1 0)
            22 -> Scene3d.quad material
                 (Point3d.meters 0 -1 0)
                 (Point3d.meters -1 -1 0)
                 (Point3d.meters -1 -1 1)
                 (Point3d.meters 0 -1 1)
            23 -> Scene3d.quad material
                 (Point3d.meters 0 -1 0)
                 (Point3d.meters 0 -1 -1)
                 (Point3d.meters -1 -1 -1)
                 (Point3d.meters -1 -1 0)
            24 -> Scene3d.quad material
                 (Point3d.meters 0 -1 0)
                 (Point3d.meters 1 -1 0)
                 (Point3d.meters 1 -1 -1)
                 (Point3d.meters 0 -1 -1)
            _ -> Scene3d.quad material (Point3d.meters 1 0 0)
                 (Point3d.meters 1 0 1)
                 (Point3d.meters 1 -1 1)
                 (Point3d.meters 1 -1 0)

view : Model -> Html Msg
view model =
    let
        panels = List.map panelView model.conf
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 5 5
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Html.div[]
        [
         Html.div[]
             [Html.button [Html.Events.onClick Front]
                  [Html.text "F"]
             ,Html.button [Html.Events.onClick Left]
                 [Html.text "L"]                  
             ,Html.button [Html.Events.onClick Top]
                 [Html.text "T"]                  
             ]
        ,Scene3d.sunny
            { camera = camera
            , clipDepth = Length.meters 0.05
            , dimensions = ( Pixels.int 500, Pixels.int 500 )
            , background = Scene3d.transparentBackground
            , entities = []++panels
            , shadows = True
            , upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xz (Angle.degrees -120)
            }
        ]

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 500 Elapsed

    
