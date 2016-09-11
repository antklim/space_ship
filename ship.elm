module Ship exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MODEL

type alias Model =
  { position : Float
  , velocity : Int -- 0, 1 - moving right, -1 - moving left
  , shooting : Bool
  }

init : Model
init =
  { position = 0
  , velocity = 0
  , shooting = False
  }


-- UPDATE

type Msg
  = Move Int
  | Shoot Bool
  | Tick Float

update : Msg -> Model -> Model
update msg model =
  case msg of
    Move newVelocity ->
      { model | velocity = newVelocity }

    Shoot newShooting ->
      { model | shooting = newShooting }

    Tick dt ->
      { model | position = model.position + dt * toFloat model.velocity }


-- VIEW

view : Model -> Html Msg
view model =
  Svg.svg [ viewBox "0 0 60 60", width "300", height "300" ]
    [ rect [ x "0"
           , y "0"
           , width "60"
           , height "60"
           , stroke "black"
           , strokeOpacity "0.5"
           , fillOpacity "0" ] []
    , rect [ x "28", y "58.5", width "3", height "1", fill "#0B79CE" ] []
    ]
