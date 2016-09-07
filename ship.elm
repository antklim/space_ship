module Ship exposing (Model, Msg, init, update, view)

-- MODEL

type alias Ship =
  { position : Float
  , velocity : Float -- 0, 1 - moving right, -1 - moving left
  , shooting : Bool
  }

type alias Keys =
  { x : Int -- movements -1, 0, 1
  , y : Int -- shooting 0, 1
  }

type alias Model =
  { ship : Ship
  }

initShip : Ship
initShip =
  { position = 0
  , velocity = 0
  , shooting = False
  }

init : Model
init =
  { ship = initShip
  }


-- UPDATE
-- VIEW


applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  { ship | position = ship.position + ship.velocity * dt }

updateVelocity : Float -> Ship -> Ship
updateVelocity newVelocity ship =
  { ship | velocity = newVelocity }

updateShooting : Bool -> Ship -> Ship
updateShooting newShooting ship =
  { ship | shooting = newShooting }

updateShip : Float -> Keys -> Ship -> Ship
updateShip dt keys ship =
  let
    newVel = toFloat keys.x
    isShooting = keys.y > 0

  in
    updateVelocity newVel (updateShooting isShooting (applyPhysics dt))
