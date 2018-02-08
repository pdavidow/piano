module Lib -- todo rename ???
    ( Direction(..)
    , octaveSemitoneCount
    ) where


data Direction = Up | Down deriving (Eq, Show)


octaveSemitoneCount :: Int
octaveSemitoneCount = 12