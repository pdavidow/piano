module DrivingSpeed
    ( DrivingSpeed(..)
    , toSafeness
    , toSafeness'
    , driverFeedback
    )
    where

import Safeness ( Safeness(..), fmap )
import TextPhrase ( TextPhrase, make, elagantize, emphasize )


newtype DrivingSpeed = DrivingSpeed Int deriving (Eq, Ord, Show) 


toSafeness' :: DrivingSpeed -> a -> Safeness a
toSafeness' (DrivingSpeed n) a =
    let
        fn = 
            if n <= 55 then
                VerySafe 
            else if n <= 70 then
                Safe 
            else
                Unsafe 
    in
        fn a


toSafeness :: DrivingSpeed -> Safeness DrivingSpeed
toSafeness s = toSafeness' s s


driverFeedback :: DrivingSpeed -> TextPhrase
driverFeedback x =
    let
        safeness = show <$> toSafeness x
    in
        case safeness of
            (Unsafe s) -> emphasize True $ TextPhrase.make s
            (Safe s) -> TextPhrase.make s
            (VerySafe s) -> elagantize True $ TextPhrase.make s