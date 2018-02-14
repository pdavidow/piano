module Crypto
    ( actualSafeness )
    where

import Safeness ( Safeness, (>>=) )
import CryptoDedicatedStorage ( CryptoDedicatedStorage(..), toSafeness )
import CryptoCurrency ( CryptoCurrency, toSafeness )
 

actualSafeness :: CryptoDedicatedStorage -> Safeness CryptoCurrency
actualSafeness s = 
    CryptoDedicatedStorage.toSafeness s >>= storageRating
    
 
storageRating :: CryptoDedicatedStorage -> Safeness CryptoCurrency
storageRating storage =
    case storage of
        HardwareWallet c -> CryptoCurrency.toSafeness c
        OnlineWallet c -> CryptoCurrency.toSafeness c
        Exchange c -> CryptoCurrency.toSafeness c