module DrivingWeather
    ( DrivingWeather(..)
    , toSafeness
    , toSafeness'
    )
    where

import Safeness ( Safeness(..) )


data DrivingWeather
    = ClearSkies
    | LightRain
    | HeavyRain
    | Hail
    deriving (Eq, Show) 


toSafeness' :: DrivingWeather -> a -> Safeness a
toSafeness' w a =
    let
        fn = 
            case w of
                ClearSkies -> VerySafe
                LightRain -> Safe 
                HeavyRain -> Unsafe 
                Hail -> Unsafe 
    in
        fn a


toSafeness :: DrivingWeather -> Safeness DrivingWeather
toSafeness w = toSafeness' w w
    