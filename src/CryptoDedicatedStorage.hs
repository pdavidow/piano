module CryptoDedicatedStorage
    ( CryptoDedicatedStorage(..)
    , toSafeness
    , toSafeness'
    )
    where

import Safeness ( Safeness(..) )
import CryptoCurrency ( CryptoCurrency )


data CryptoDedicatedStorage
    = HardwareWallet CryptoCurrency
    | OnlineWallet CryptoCurrency
    | Exchange CryptoCurrency
    deriving (Eq, Show)


toSafeness' :: CryptoDedicatedStorage -> b -> Safeness b
toSafeness' c a =
    let
        fn = 
            case c of
                HardwareWallet _ -> VerySafe
                OnlineWallet _ -> Safe 
                Exchange _ -> Unsafe 
    in
        fn a


toSafeness :: CryptoDedicatedStorage -> Safeness CryptoDedicatedStorage
toSafeness c = toSafeness' c c