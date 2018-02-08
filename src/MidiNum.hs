module MidiNum
    ( MidiNum(..)
    , shiftBySemitone
    , shiftByOctave
    , basicShow
    )
    where

import Lib ( Direction(..), octaveSemitoneCount ) 


-- Treated as boundless
newtype MidiNum = MidiNum Int deriving (Eq, Ord, Show)


shiftBySemitone :: Int -> Direction -> MidiNum -> MidiNum
shiftBySemitone count direction (MidiNum n) =
    let
        operator = case direction of
            Up -> (+)
            Down -> (-)        
    in
        MidiNum $ operator n count


shiftByOctave :: Int -> Direction -> MidiNum -> MidiNum
shiftByOctave octaveCount direction midiNum = 
    shiftBySemitone (octaveCount * octaveSemitoneCount) direction midiNum


basicShow :: MidiNum -> String
basicShow (MidiNum n) = show n