module CryptoCurrency
    ( CryptoCurrency(..)
    , toSafeness
    , toSafeness'
    )
    where

import Safeness ( Safeness(..) )

data CryptoCurrency
    = SuperCoin
    | OkayCoin
    | UhohCoin
    deriving (Eq, Show)


toSafeness' :: CryptoCurrency -> a -> Safeness a
toSafeness' c a =
    let
        fn = 
            case c of
                SuperCoin -> VerySafe
                OkayCoin -> Safe 
                UhohCoin -> Unsafe 
    in
        fn a


toSafeness :: CryptoCurrency -> Safeness CryptoCurrency
toSafeness c = toSafeness' c c   