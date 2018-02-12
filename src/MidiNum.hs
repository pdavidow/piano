{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MidiNum
    ( MidiNum(..)
    , shiftBySemitone
    , shiftByOctave
    , toInt
    , basicShow
    )
    where

import Lib ( octaveSemitoneCount ) 


newtype MidiNum = MidiNum Int deriving (Enum, Eq, Ord, Show)


shiftBySemitone :: Int -> MidiNum -> MidiNum
shiftBySemitone count (MidiNum n) =
    MidiNum $ n + count 


shiftByOctave :: Int -> MidiNum -> MidiNum
shiftByOctave octaveCount midiNum = 
    shiftBySemitone (octaveCount * octaveSemitoneCount) midiNum


toInt :: MidiNum -> Int
toInt (MidiNum n) = n


basicShow :: MidiNum -> String
basicShow m = show $ toInt m