module CryptoCurrency
    ( CryptoCurrency(..)
    , toSafeness
    )
    where

import Safeness ( Safeness(..) )

data CryptoCurrency
    = SuperCoin
    | OkayCoin
    | UhohCoin
    deriving (Eq, Show)


toSafeness :: CryptoCurrency -> Safeness CryptoCurrency
toSafeness x = 
    f x 
    where f = 
            case x of
                SuperCoin -> VerySafe
                OkayCoin -> Safe 
                UhohCoin -> Unsafe 