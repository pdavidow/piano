module Instrument
    ( Instrument(..)
    , midiNumRange
    , isMonophonic
    , isPolyphonic 
    )
    where

-- https://soundprogramming.net/file-formats/midi-note-ranges-of-orchestral-instruments/


import SpannedRange ( SpannedRange(..) )
import MidiNum ( MidiNum(..) )


data Instrument 
    = Clarinet 
    | Flute 
    | Harp 
    | Harpsichord 
    | Marimba 
    | Piano 
    | Piccolo 
    | Trombone 
    | Trumpet 
    | Xylophone
    deriving (Eq, Show)


midiNumRange :: Instrument -> SpannedRange MidiNum
midiNumRange i =
     let 
        (start, end) = case i of
            Clarinet -> (50, 94)
            Flute -> (60, 96)
            Harp -> (24, 103)
            Harpsichord -> (29, 89)
            Marimba -> (45, 96)
            Piano -> (21, 108)
            Piccolo -> (74, 102)
            Trombone -> (40, 72)
            Trumpet -> (55, 82)
            Xylophone -> (65, 108)
    in
        SpannedRange (MidiNum start) (MidiNum end)


isMonophonic :: Instrument -> Bool
isMonophonic Clarinet = True
isMonophonic Flute = True
isMonophonic Piccolo = True
isMonophonic Trombone = True
isMonophonic Trumpet = True
isMonophonic _ = False


isPolyphonic :: Instrument -> Bool
isPolyphonic i = not $ isMonophonic i