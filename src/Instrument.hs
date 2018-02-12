module Instrument
    ( Instrument(..), midiNumRange )
    where

import Data.Range.Range ( Range(..) )

import MidiNum ( MidiNum(..) )


data Instrument
    = Harp
    | Harpsichord 
    | Marimba 
    | Piano 
    | Xylophone 
    deriving (Eq, Show)


midiNumRange :: Instrument -> Range MidiNum
midiNumRange i =
    -- https://soundprogramming.net/file-formats/midi-note-ranges-of-orchestral-instruments/
    let 
        (start, end) = case i of
            Harp -> (24, 103)
            Harpsichord -> (29, 89)
            Marimba -> (45, 96)
            Piano -> (21, 108)
            Xylophone -> (65, 108)
    in
        SpanRange (MidiNum start) (MidiNum end)