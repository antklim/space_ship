import Html exposing (..)
import Html.App as ShipApp
import Time exposing (..)
import Keyboard
import String exposing (append)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
  ShipApp.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Ship =
  { position : Float
  , velocity : Int
  , shooting : Bool
  }

type alias Model =
  { ship : Ship
  , leftBorder : Float
  , rightBorder : Float
  }

initShip : Float -> Int -> Bool -> Ship
initShip p v s =
  { position = p
  , velocity = v
  , shooting = s
  }

init : (Model, Cmd Msg)
init =
  ( { ship = initShip 28 0 False
    , leftBorder = 0
    , rightBorder = 117
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = KeyDowns Keyboard.KeyCode
  | KeyUps Keyboard.KeyCode
  | Tick Time

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

    Tick _ ->
      ( updatePosition model
      , Cmd.none
      )

keyDowns : Model -> Keyboard.KeyCode -> Model
keyDowns model code =
  let
    velocity =
      if code == 37 then
        -1

      else if code == 39 then
        1

      else
        model.ship.velocity

    shooting = if code == 32 then True else model.ship.shooting

  in
    { model | ship = initShip model.ship.position velocity shooting }

keyUps : Model -> Keyboard.KeyCode -> Model
keyUps model code =
  let
    velocity =
      if code == 37 || code == 39 then
        0

      else
        model.ship.velocity

    shooting = if code == 32 then False else model.ship.shooting

  in
    { model | ship = initShip model.ship.position velocity shooting }

updatePosition : Model -> Model
updatePosition model =
  let
    newPosition = model.ship.position + toFloat model.ship.velocity * 3
    position =
      if newPosition > model.rightBorder then
        model.rightBorder

      else if newPosition < model.leftBorder then
        model.leftBorder

      else
        newPosition

  in
    { model | ship = initShip position model.ship.velocity model.ship.shooting }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDowns
    , Keyboard.ups KeyUps
    , Time.every (100 * millisecond) Tick
    ]


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ Html.text (append "Velocity: " (toString model.ship.velocity))
    , br [] []
    , Html.text (append "Shooting: " (toString model.ship.shooting))
    , br [] []
    , Html.text (append "Position: " (toString model.ship.position))
    , hr [] []
    , Svg.svg [ viewBox "0 0 60 60", width "900", height "300" ]
        [ rect [ x "0"
               , y "0"
               , width "120"
               , height "60"
               , stroke "black"
               , strokeOpacity "0.5"
               , fillOpacity "0" ] []
        , rect [ x (toString model.ship.position)
               , y "58.5"
               , width "3"
               , height "1"
               , fill (shipColor model) ] []
        ]
    ]

shipColor : Model -> String
shipColor model =
  if model.ship.shooting then "red" else "#0B79CE"
