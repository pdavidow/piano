module Lib
    ( Direction(..)
    , Elagantized
    , Emphasized
    , elagantize
    , emphasize
    , octaveSemitoneCount
    ) where
   

data Direction = Up | Down deriving (Eq, Show)


octaveSemitoneCount :: Int
octaveSemitoneCount = 12


class Emphasized a where
    emphasize :: Bool -> a -> a


class Elagantized a where
    elagantize :: Bool -> a -> a