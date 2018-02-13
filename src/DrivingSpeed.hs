module DrivingSpeed
    ( DrivingSpeed(..)
    , driverFeedback
    )
    where

import Safeness ( Safeness(..), fmap )
import TextPhrase ( TextPhrase, make, elagantize, emphasize )


type Speed = Int


speedLimit :: Speed
speedLimit = 55


newtype DrivingSpeed = DrivingSpeed Speed deriving (Eq, Ord, Show) 


toSafeness :: DrivingSpeed -> Safeness Speed
toSafeness (DrivingSpeed x) =
    if x < 0 then 
        Unsafe x
    else if x == 0 then 
        VerySafe x
    else if x <= speedLimit then
        Safe x
    else
        Unsafe x


driverFeedback :: DrivingSpeed -> TextPhrase
driverFeedback x =
    let
        safeness = show <$> toSafeness x
    in
        case safeness of
            (Unsafe s) -> emphasize True $ TextPhrase.make s
            (Safe s) -> TextPhrase.make s
            (VerySafe s) -> elagantize True $ TextPhrase.make s