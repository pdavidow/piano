module DrivingTrip
    ( isGoodTrip )
    where

import Safeness ( Safeness(..), isNotUnsafe, (<*>) )
import DrivingSpeed ( DrivingSpeed(..), toSafeness )
import DrivingWeather ( DrivingWeather(..), toSafeness' )


data TripOutcome
    = Good
    | Bad


type SpeedToOutcome = ( DrivingSpeed -> TripOutcome )


tripOutcome :: SpeedToOutcome
tripOutcome speed =
    case isNotUnsafe $ DrivingSpeed.toSafeness speed of
        True -> Good
        False -> Bad


tripReliability :: SpeedToOutcome -> DrivingWeather -> Safeness SpeedToOutcome
tripReliability fn w = 
    DrivingWeather.toSafeness' w fn


isGoodTrip :: DrivingWeather -> DrivingSpeed -> Bool
isGoodTrip weather speed =
    let
        calcSafeness = tripReliability tripOutcome weather
        speedSafeness = DrivingSpeed.toSafeness speed
    in
        isNotUnsafe $ calcSafeness <*> speedSafeness
          