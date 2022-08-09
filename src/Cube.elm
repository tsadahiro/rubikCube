module Cube exposing (main)

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import LineSegment3d
import Viewpoint3d
import Browser
import Time
import Quantity
import Axis3d

type alias Model = {angleX: Angle
                   ,angleY: Angle}

type Msg = Elapsed Time.Posix
    
main = Browser.element {init=init
                       ,update=update
                       ,view=view
                       ,subscriptions=subscriptions}

       
init: () -> (Model, Cmd Msg)       
init _ = (Model Quantity.zero Quantity.zero
       ,Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Elapsed t ->
            ({model |
                  angleX = Debug.log "" <| Quantity.plus model.angleX (Angle.degrees 3)}
            ,Cmd.none)


view: Model -> Html Msg
view model =
    let
        xAxis = Axis3d.x
        material = Material.nonmetal
                   { baseColor = Color.black
                   , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                   }
        materialBlue = Material.nonmetal
                   { baseColor = Color.blue
                   , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                   }
        materialRed = Material.nonmetal
                   { baseColor = Color.red
                   , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                   }
        materialGreen = Material.nonmetal
                   { baseColor = Color.green
                   , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                   }
        materialOrange = Material.nonmetal
                   { baseColor = Color.orange
                   , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                   }
    in
    Scene3d.sunny
        {shadows=False
        ,sunlightDirection = Direction3d.yz (Angle.degrees 135)
        , upDirection = Direction3d.z
        ,entities =
            [ Scene3d.rotateAround xAxis model.angleX <|
                  Scene3d.group [
                       Scene3d.quad materialBlue
                           (Point3d.meters 0 1 0)
                           (Point3d.meters 0 1 1)
                           (Point3d.meters 1 1 1)
                           (Point3d.meters 1 1 0)
                      ,Scene3d.quad materialBlue
                           (Point3d.meters -1 1 0)
                           (Point3d.meters -1 1 1)
                           (Point3d.meters 0 1 1)
                           (Point3d.meters 0 1 0)
                      ,Scene3d.quad materialBlue
                           (Point3d.meters -1 1 -1)
                           (Point3d.meters -1 1 0)
                           (Point3d.meters 0 1 0)
                           (Point3d.meters 0 1 -1)
                      ,Scene3d.quad materialBlue
                           (Point3d.meters 0 1 -1)
                           (Point3d.meters 0 1 0)
                           (Point3d.meters 1 1 0)
                           (Point3d.meters 1 1 -1)
                           ]
                  ,Scene3d.rotateAround xAxis model.angleX <|
                      Scene3d.group [
                       Scene3d.quad materialBlue
                           (Point3d.meters 0 0 1)
                           (Point3d.meters 1 0 1)
                           (Point3d.meters 1 -1 1)
                           (Point3d.meters 0 -1 1)
                      ,Scene3d.quad materialRed
                           (Point3d.meters 1 0 0)
                           (Point3d.meters 1 -1 0)
                           (Point3d.meters 1 -1 1)
                           (Point3d.meters 1 0 1)
                      ,Scene3d.quad materialOrange
                           (Point3d.meters 0 -1 0)
                           (Point3d.meters 1 -1 0)
                           (Point3d.meters 1 -1 1)
                           (Point3d.meters 0 -1 1)
                      ]
                  ,Scene3d.group [
                       Scene3d.quad materialBlue
                           (Point3d.meters 0 0 1)
                           (Point3d.meters -1 0 1)
                           (Point3d.meters -1 1 1)
                           (Point3d.meters 0 1 1)
                      ,Scene3d.quad material
                           (Point3d.meters 0 0 0)
                           (Point3d.meters 0 1 0)
                           (Point3d.meters 0 1 1)
                           (Point3d.meters 0 0 1)
                      ,Scene3d.quad materialGreen
                           (Point3d.meters 0 1 0)
                           (Point3d.meters -1 1 0)
                           (Point3d.meters -1 1 1)
                           (Point3d.meters 0 1 1)
                      ,Scene3d.quad material
                           (Point3d.meters 0 0 0)
                           (Point3d.meters -1 0 0)
                           (Point3d.meters -1 1 0)
                           (Point3d.meters 0 1 0)
                      ,Scene3d.quad material
                           (Point3d.meters 0 0 0)
                           (Point3d.meters 0 1 0)
                           (Point3d.meters 0 1 1)
                           (Point3d.meters 0 0 1)
                      ,Scene3d.quad material
                           (Point3d.meters 0 0 0)
                           (Point3d.meters -1 0 0)
                           (Point3d.meters -1 0 1)
                           (Point3d.meters 0 0 1)
                      ]
            ,Scene3d.lineSegment 
                  (Material.color Color.black)
                (LineSegment3d.fromEndpoints
                  ( Point3d.meters -2 0 0
                  , Point3d.meters 2 0 0
                  ))
            ,Scene3d.lineSegment 
                  (Material.color Color.black)
                (LineSegment3d.fromEndpoints
                  ( Point3d.meters 0 -2 0
                  , Point3d.meters 0 2 0
                  ))
            ,Scene3d.lineSegment 
                  (Material.color Color.black)
                (LineSegment3d.fromEndpoints
                  ( Point3d.meters 0 0 -2
                  , Point3d.meters 0 0 2 
                  ))
            ]
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 5 5
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        }
    
      
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 500 Elapsed

