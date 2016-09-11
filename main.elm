import Html exposing (..)
import Html.App as ShipApp
import Time exposing (..)
import Keyboard
import String exposing (..)
import Debug exposing (..)


import Ship

main =
  ShipApp.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model =
  { ship : Ship.Model
  , direction : String
  , moving : Bool
  , shooting : Bool
  }

init : (Model, Cmd Msg)
init =
  ( { ship = Ship.init
    , direction = "none"
    , moving = False
    , shooting = False
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = KeyDowns Keyboard.KeyCode
  | KeyUps Keyboard.KeyCode
  | ViewShip Ship.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDowns code ->
      ( keyDowns model code
      , Cmd.none
      )

    KeyUps code ->
      ( keyUps model code
      , Cmd.none
      )

    ViewShip msg ->
      ( { model | ship = Ship.update msg model.ship }
      , Cmd.none
      )

keyDowns : Model -> Keyboard.KeyCode -> Model
keyDowns model code =
  let
    direction =
      if code == 37 then
        "left"

      else if code == 39 then
        "right"

      else
        model.direction

    shooting =
      if code == 32 then True else model.shooting

    moving =
      if code == 37 || code == 39 then True else model.moving

  in
    { model
      | direction = direction
      , moving = moving
      , shooting = shooting
    }

keyUps : Model -> Keyboard.KeyCode -> Model
keyUps model code =
  let
    direction =
      if code == 37 || code == 39 then
        "none"

      else
        model.direction

    shooting =
      if code == 32 then False else model.shooting

    moving =
      if code == 37 || code == 39 then False else model.moving

  in
    { model
      | direction = direction
      , moving = moving
      , shooting = shooting
    }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDowns
    , Keyboard.ups KeyUps
    ]


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ text (append "Mooving: " (toString model.moving))
    , br [] []
    , text (append "Direction: " (toString model.direction))
    , br [] []
    , text (append "Shooting: " (toString model.shooting))
    , br [] []
    , ShipApp.map ViewShip (Ship.view model.ship)
    ]
